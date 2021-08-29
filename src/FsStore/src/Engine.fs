namespace FsStore

open System
open System.Collections.Generic
open Fable.Core.JsInterop
open Fable.Core
open FsJs
open FsStore.Bindings
open FsStore.Bindings.Jotai
open FsStore.Model
open FsCore
open FsStore.State

#nowarn "40"


module Engine =
    type UpdateFn<'State, 'Command, 'Event> =
        Getter<obj> -> Setter<obj> -> 'State -> 'Command -> JS.Promise<'State * Message<'Command, 'Event> list>

    let inline consumeCommands (updateFn: UpdateFn<_, _, _>) state getter setter commands =
        promise {
            //            let logger = Atom.get getter Selectors.logger

            let rec loop state commands processedMessages =
                promise {
                    match commands with
                    | command :: commands ->
                        let! state, processedMessages' = updateFn getter setter state command

                        let commands', events =
                            processedMessages'
                            |> List.partition
                                (function
                                | Message.Command _ -> true
                                | _ -> false)

                        let newCommands =
                            commands'
                            |> List.choose
                                (function
                                | Message.Command command -> Some command
                                | _ -> None)

                        return! loop state (newCommands @ commands) (processedMessages @ events)
                    | [] -> return state, processedMessages
                }

            let! newState, processedMessages = loop state commands []


            let getDebugInfo () =
                $"commands={commands} newState={newState} processedMessages={processedMessages}"

            let addTimestamp fn getDebugInfo =
                Profiling.addTimestamp
                    (fun () -> $"{nameof FsStore} | Engine.consumeCommands {fn ()} | {getDebugInfo ()}")

            addTimestamp (fun () -> "[ ](_1)") getDebugInfo

            return
                newState,
                processedMessages
                |> List.choose
                    (function
                    | Message.Event event -> Some event
                    | _ -> None)
        }


    let mutable lastStore: (Getter<obj> * Setter<obj>) option = None

    let inline wrapAtomWithState<'A, 'S>
        (stateFn: Getter<obj> -> 'S option)
        (mount: Getter<obj> -> Setter<obj> -> 'S -> ('A -> unit) -> JS.Promise<unit>)
        (unmount: Getter<obj> -> Setter<obj> -> 'S -> unit)
        (atom: AtomConfig<'A>)
        =
        let storeAtomPath =
            if Atom.isRegistered (AtomReference.Atom atom) then
                Some (Atom.query (AtomReference.Atom atom))
            else
                None

        let mutable lastState: 'S option = None

        let mutable mounted = false

        let getDebugInfo () =
            $" | atom={atom} mounted={mounted} storeAtomPath={storeAtomPath |> Option.map StoreAtomPath.AtomPath} lastState.IsSome={lastState.IsSome} "

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp
                (fun () -> $"{nameof FsStore} | Engine.wrapAtomWithState {fn ()} | {getDebugInfo ()}")

        addTimestamp (fun () -> "[ constructor ](g1)") getDebugInfo

        let mutable lastSetAtom = None

        let getState () =
            match lastStore with
            | Some (getter, setter) ->
                let state =
                    match stateFn getter with
                    | Some state -> Some state
                    | None -> lastState

                lastState <- state

                match state with
                | Some state -> Some (getter, setter, state)
                | None -> None
            | _ -> None

        let newMount () =
            promise {
                match getState () with
                | Some (getter, setter, state) ->
                    addTimestamp (fun () -> "[ newMount ](g2) invoking mount") getDebugInfo

                    mounted <- true
                    do! mount getter setter state lastSetAtom.Value
                | None -> addTimestamp (fun () -> "[ newMount ](g3) skipping, no state") getDebugInfo
            }

        let newUnmount () =
            if mounted then
                match getState () with
                | Some (getter, setter, state) ->
                    addTimestamp (fun () -> "[ newUnmount ](g4) invoking unmount") getDebugInfo

                    mounted <- false
                    lastState <- None
                    unmount getter setter state
                | None ->
                    addTimestamp
                        (fun () -> "[ newUnmount ](g5) invoking unmount skipping, no state. (should unmount here???)")
                        getDebugInfo

        let refreshInternalState getter =
            if lastStore.IsNone then lastStore <- Atom.get getter Selectors.store

            let logger = Atom.get getter Selectors.logger
            Logger.State.lastLogger <- logger

            let newState = stateFn getter

            let getDebugInfo () =
                $"newState.IsSome={newState.IsSome} lastSetAtom.IsNone={lastSetAtom.IsNone} {getDebugInfo ()}"

            if lastSetAtom.IsNone then
                addTimestamp
                    (fun () -> "[ refreshInternalState ](g5) skipping mount/unmount. lastSetAtom not found")
                    getDebugInfo
            else
                match newState with
                | Some _ ->
                    addTimestamp (fun () -> "[ refreshInternalState ](g6) invoking newMount") getDebugInfo
                    lastState <- newState
                    newMount () |> Promise.start
                | None ->
                    addTimestamp (fun () -> "[ refreshInternalState ](g7) invoking newUnmount") getDebugInfo
                    newUnmount ()

        let wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    refreshInternalState getter

                    let result = Atom.get getter atom
                    let getDebugInfo () = $"result={result}  {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ wrapper.get() ](g8)") getDebugInfo

                    result)
                (fun getter setter newValue ->
                    refreshInternalState getter

                    let getDebugInfo () =
                        $"newValue={newValue}  {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ wrapper.set() ](g9)") getDebugInfo
                    Atom.set setter atom newValue)
            |> Atom.addSubscription
                true
                (fun setAtom ->
                    addTimestamp (fun () -> "[ addSubscription mount ](g10) invoking newMount") getDebugInfo
                    lastSetAtom <- Some setAtom
                    newMount ())
                (fun () ->
                    addTimestamp (fun () -> "[ addSubscription unmount ](g11) invoking newUnmount") getDebugInfo
                    newUnmount ())

        match storeAtomPath with
        | Some storeAtomPath -> wrapper |> Atom.register storeAtomPath
        | None -> wrapper


    let inline wrapAtom<'A>
        (mount: Getter<obj> -> Setter<obj> -> ('A -> unit) -> JS.Promise<unit>)
        (unmount: Getter<obj> -> Setter<obj> -> unit)
        (atom: AtomConfig<'A>)
        =
        wrapAtomWithState
            (fun _ -> Some ())
            (fun getter setter _ -> mount getter setter)
            (fun getter setter _ -> unmount getter setter)
            atom


    let inline wrapAtomWithInterval defaultValue interval atom =
        let mutable intervalHandle = -1
        let mutable lastValue = None

        let getDebugInfo () =
            $" interval={interval} defaultValue={defaultValue} lastValue={lastValue} timeout={intervalHandle} "

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp
                (fun () -> $"{nameof FsStore} | Engine.wrapAtomWithInterval {fn ()} | {getDebugInfo ()}")

        let cache = Atom.Primitives.atom defaultValue

        let wrapper =
            atom
            |> wrapAtom
                (fun getter setter _setAtom ->
                    promise {
                        addTimestamp (fun () -> "[ wrapper.mount() ](h1)") getDebugInfo

                        let fn () =
                            addTimestamp (fun () -> "[ wrapper.mount.fn() ](h2) interval fn") getDebugInfo

                            if intervalHandle >= 0 then
                                let atomValue = Atom.get getter atom

                                if Some atomValue |> Object.compare lastValue |> not then
                                    let getDebugInfo () =
                                        $"atomValue={atomValue} {getDebugInfo ()}"

                                    addTimestamp
                                        (fun () -> "[ wrapper.mount.fn() ](h3) interval fn. triggering new value")
                                        getDebugInfo

                                    Atom.set setter cache atomValue
                                    lastValue <- Some atomValue

                        if intervalHandle = -1 then fn ()
                        intervalHandle <- JS.setInterval fn interval
                    })
                (fun _getter _setter ->
                    //                let logger = Logger.State.lastLogger
                    addTimestamp (fun () -> "[ wrapper.unmount() ](h4) ") getDebugInfo

                    if intervalHandle >= 0 then JS.clearTimeout intervalHandle
                    intervalHandle <- -1)

        wrapper?init <- defaultValue

        wrapper


    //    let inline wrapAtomWithSubscription stateFn mount unmount atom =
//        let storeAtomPath = Atom.query (AtomReference.Atom atom)
//        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath
//
//        let getDebugInfo () =
//            $"""
//        | atom={atom} atomPath={atomPath} """
//
//
//        atom
//        |> wrapAtomWithState
//            (fun getter ->
//                let newState = stateFn getter
//                newState)
//            (fun _getter _setter state _setAtom ->
//                promise {
//                    Profiling.addTimestamp (fun () -> $"Engine.wrapAtomWithSubscription. onMount(). {getDebugInfo ()}"
//                    do! mount state
//                })
//            (fun _getter _setter _state ->
//                Profiling.addTimestamp (fun () -> $"Engine.wrapAtomWithSubscription onUnmount() {getDebugInfo ()}"
//                unmount ()
//                ())


    //    let inline subscribe (gun: IGunChainReference) fn =
//        gun.on
//            (fun data (GunNodeSlice key) ->
//                promise {
//                    Logger.logDebug
//                        (fun () ->
//                            if key = "devicePing" then
//                                null
//                            else
//                                $"subscribe.on() data. batching...data={data} key={key}")
//
//                    fn data
//                })
//
//        Object.newDisposable
//            (fun () ->
//                printfn "subscribe.on() data. Dispose promise observable."
//                gun.off () |> ignore)
//        |> Promise.lift





    //    let getHubSubscription storeAtomPath  adapterOptions =
//                            match storeAtomPath|>StoreAtomPath.AtomPath, adapterOptions with
//                            | AtomPath atomPath, Atom.AdapterOptions.Hub (hubUrl, alias) ->
//                                try
//                                    let storeRoot, collection =
//                                        match atomPath |> String.split "/" |> Array.toList with
//                                        | storeRoot :: [ _ ] -> Some storeRoot, None
//                                        | storeRoot :: collection :: _ -> Some storeRoot, Some collection
//                                        | _ -> None, None
//
//                                    //                                hubSubscriptionMap
//                                    match storeRoot, collection with
//                                    | Some storeRoot, Some collection ->
//                                        let collectionPath = alias, StoreRoot storeRoot, Collection collection
//
//                                        match Selectors.Hub.hubSubscriptionMap.TryGetValue collectionPath with
//                                        | true, _sub ->
//                                            Logger.logError
//                                                (fun () -> $"Store.selectAtomSyncKeys sub already present key={key}")
//
//                                            None
//                                        | _ ->
//                                            let handle items =
//                                                if items |> Array.isEmpty |> not then
//                                                    Logger.logTrace
//                                                        (fun () ->
//                                                            $"Store.selectAtomSyncKeys subscribe. hub data received (inside disposable)
//                                                                          setting keys locally. items.Length={items.Length}
//                                                                          atomPath={atomPath} syncEngine.atomPath={syncEngine.GetAtomPath ()} key={key} {getDebugInfo ()} ")
//
//                                                    batchKeysAtom
//                                                        (Guid.newTicksGuid (), items |> Array.map onFormat)
//                                                        BatchKind.Replace
//                                                else
//                                                    Logger.logTrace
//                                                        (fun () ->
//                                                            $"Store.selectAtomSyncKeys subscribe. skipping key batch. items.Length=0
//                                                                          atomPath={atomPath} syncEngine.atomPath={syncEngine.GetAtomPath ()} key={key} {getDebugInfo ()} ")
//
//
//                                            Selectors.Hub.hubSubscriptionMap.[collectionPath] <- handle
//
//                                            Gun.batchHubSubscribe
//                                                hub
//                                                (Sync.Request.Filter collectionPath)
//                                                (Guid.newTicksGuid ())
//                                                (fun (ticks, response: Sync.Response) ->
//                                                    Logger.logTrace
//                                                        (fun () ->
//                                                            $"Store.selectAtomSyncKeys [wrapper.next() HUB keys stream subscribe] ticks={ticks} {getDebugInfo ()} response={response}")
//
//                                                    promise {
//                                                        match response with
//                                                        | Sync.Response.FilterResult items ->
//                                                            handle items
//
//                                                            Logger.logTrace
//                                                                (fun () ->
//                                                                    $"Store.selectAtomSyncKeys [wrapper.on() HUB KEYS subscribe] atomPath={atomPath} items={JS.JSON.stringify items} {getDebugInfo ()} ")
//                                                        | response ->
//                                                            Logger.logError
//                                                                (fun () ->
//                                                                    $"Store.selectAtomSyncKeys Gun.batchHubSubscribe invalid response={response}")
//
//                                                        return! newHashedDisposable ticks
//                                                    })
//                                                (fun _ex ->
//                                                    Selectors.Hub.hubSubscriptionMap.Remove collectionPath
//                                                    |> ignore)
//
//                                            Some ()
//                                    | _ ->
//                                        Logger.logError
//                                            (fun () ->
//                                                $"Store.selectAtomSyncKeys #123561 invalid atom path atomPath={atomPath}")
//
//                                        None
//                                with
//                                | ex ->
//                                    Logger.logError
//                                        (fun () -> $"Store.selectAtomSyncKeys hub.filter, error={ex.Message}")
//
//                                    None
//                            | _ ->
//                                Logger.logTrace
//                                    (fun () ->
//                                        $"Store.selectAtomSyncKeys subscribe. hub skipped. hub sync options disabled. (inside disposable) key={key} {getDebugInfo ()}")
//
//                                None

    //    let inline createRegisteredAtomWithGroup
//        (storeAtomPath: StoreAtomPath)
//        (defaultGroup: 'TGroup, defaultValue: 'A)
//        : AtomConfig<TicksGuid * 'TGroup * 'A> =
//
//        let defaultValue = Guid.Empty, (defaultGroup, defaultValue)
//
//        let getDebugInfo () =
//            $"storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath} defaultValue={defaultValue}"
//
//        Profiling.addTimestamp
//            (fun () -> $"+13.1c Engine.createRegisteredAtomWithGroup [ constructor ] {getDebugInfo ()}")
//
//        let groupMapAtom = Atom.atomFamilyAtom (fun (_alias: Gun.Alias option) -> defaultValue |> List.singleton)
//
//        let rec lastSyncValueByTypeAtom =
//            Atom.Primitives.readSelectorFamily
//                (fun (alias: Gun.Alias option) getter ->
//                    let groupMap = Atom.get getter (groupMapAtom alias)
//                    groupValues groupMap)
//
//        let wrapper =
//            Atom.Primitives.selector
//                (fun getter ->
//                    //                        let value = Atom.get getter subscriptions.[adapterType]
//
//                    //                        syncEngine.SetProviders getter wrapper
//                    let alias = Atom.get getter Selectors.Gun.alias
//
//                    let userAtom = lastSyncValueByTypeAtom alias
//                    let lastSyncValueByType = Atom.get getter userAtom
//
//                    let ticks, (group: 'TGroup, result) =
//                        lastSyncValueByType
//                        |> Seq.sortByDescending fst
//                        |> Seq.head
//
//                    Profiling.addTimestamp
//                        (fun () ->
//                            $"+13c Engine.createRegisteredAtomWithGroup wrapper.get()  alias={alias} result={result} lastSyncValueByType={Json.encodeWithNull lastSyncValueByType} {getDebugInfo ()}")
//
//                    ticks, group, result)
//                (fun getter setter (ticks, group: 'TGroup, newValue) ->
//                    let alias = Atom.get getter Selectors.Gun.alias
//
//                    Atom.change
//                        setter
//                        (groupMapAtom alias)
//                        (fun oldGroupValueList ->
//                            let newGroupValueList =
//                                (ticks, (group, newValue))
//                                :: (oldGroupValueList
//                                    |> List.filter (fun (_, (group', _)) -> group' <> group))
//
//                            Profiling.addTimestamp
//                                (fun () ->
//                                    $"+12c Engine.createRegisteredAtomWithGroup wrapper.set() ticks={ticks} alias={alias} newGroupValueList={newGroupValueList} oldGroupValueList={oldGroupValueList} {getDebugInfo ()} newValue={newValue} ")
//
//                            newGroupValueList))
//            |> Atom.register storeAtomPath
//
//        wrapper?init <- defaultValue
//
//        wrapper

    let memoryAdapterOptions = Some Atom.AdapterOptions.Memory

    let inline getAdapterOptions getter adapterType =
        match adapterType with
        | Atom.AdapterType.Gun -> Atom.get getter Selectors.Gun.adapterOptions
        | Atom.AdapterType.Hub -> Atom.get getter Selectors.Hub.adapterOptions
        | Atom.AdapterType.Memory -> memoryAdapterOptions


    let inline newHashedDisposable (ticks: TicksGuid) =
        promise {
            Logger.logDebug (fun () -> $"BaseStore.newHashedDisposable constructor ticks={ticks}")

            return
                Object.newDisposable
                    (fun () -> Logger.logDebug (fun () -> $"BaseStore.newHashedDisposable disposing... ticks={ticks}"))
        }

    let inline batchPutFromUi<'A when 'A: equality> (gunAtomNode, privateKeys, ticks, newValue, onPut) =
        let getDebugInfo () = $" ticks={ticks} newValue={newValue}  "

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.batchPutFromUi {fn ()} | {getDebugInfo ()}")

        Batcher.batch (
            Batcher.BatchType.Set (
                ticks,
                (fun ticks ->
                    promise {
                        let! newValueJson =
                            promise {
                                if newValue |> Js.ofNonEmptyObj |> Option.isNone then
                                    return null
                                else
                                    let! (Gun.EncryptedSignedValue encrypted) =
                                        Gun.userEncode<TicksGuid * 'A> privateKeys (ticks, newValue)

                                    return encrypted
                            }

                        let! putResult =
                            Gun.put
                                gunAtomNode
                                (Gun.GunValue.EncryptedSignedValue (Gun.EncryptedSignedValue newValueJson))

                        let getDebugInfo () =
                            $"putResult={putResult} {getDebugInfo ()}"

                        addTimestamp (fun () -> "[ batchSetFn ](i1)") getDebugInfo

                        if putResult then onPut ()
                        ()
                    })
            )
        )

    let inline getAdapterSubscription<'A when 'A: equality> storeAtomPath adapterType =
        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

        let getDebugInfo () =
            $"atomPath={atomPath} adapterType={adapterType}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp
                (fun () -> $"{nameof FsStore} | Engine.getAdapterSubscription {fn ()} | {getDebugInfo ()}")

        match adapterType with
        | Atom.AdapterType.Gun ->
            (fun getter _setter adapterOptions adapterSetAtom ->
                match adapterOptions with
                | Atom.AdapterOptions.Gun (_peers, alias) ->
                    let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (Some alias, atomPath))
                    let privateKeys = Atom.get getter Selectors.Gun.privateKeys

                    let getDebugInfo () =
                        $"gunAtomNode={gunAtomNode} privateKeys={privateKeys} adapterOptions={adapterOptions} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ ====> mountFn ](j1) gun") getDebugInfo

                    match gunAtomNode, privateKeys with
                    | Some gunAtomNode, Some privateKeys ->
                        addTimestamp (fun () -> "[ ||||||||| mountFn ](j2) gun. will batch subscribe") getDebugInfo

                        let debouncedAdapterSetAtom =
                            Js.debounce
                                (fun value ->
                                    let getDebugInfo () = $"value={value} {getDebugInfo ()}"

                                    addTimestamp
                                        (fun () -> "[ ********> mountFn ](j3) gun. debounced on subscribe data")
                                        getDebugInfo

                                    adapterSetAtom value)
                                0

                        Gun.batchSubscribe
                            gunAtomNode
                            (Guid.newTicksGuid ())
                            (fun (subscriptionTicks, gunValue) ->
                                promise {
                                    try
                                        let! newValue = Gun.userDecode<TicksGuid * 'A> privateKeys gunValue

                                        addTimestamp
                                            (fun () ->
                                                "[ ||==> setAdapterValue ](j4-1) invoking debouncedSetAtom. inside gun.on() ")
                                            getDebugInfo

                                        debouncedAdapterSetAtom (false, newValue |> Option.defaultValue (unbox null))
                                    with
                                    | ex ->
                                        Logger.logError
                                            (fun () ->
                                                $"Engine.getAtomAdapter. gun subscribe data error. ex={ex.Message} gunValue={gunValue} subscriptionTicks={subscriptionTicks} {getDebugInfo ()}")

                                        Logger.consoleError [| ex |]
                                })

                        let debouncedBatchPutFromUi = Js.debounce batchPutFromUi 0

                        let setAdapterValue (fromUi: bool, (lastTicks, lastValue)) =
                            let getDebugInfo () =
                                $"fromUi={fromUi} lastTicks={lastTicks} lastValue={lastValue} {getDebugInfo ()}"


                            addTimestamp
                                (fun () -> "[ ||==> setAdapterValue ](j4-2) invoking debouncedBatchPutFromUi  ")
                                getDebugInfo

                            if fromUi then
                                debouncedBatchPutFromUi (
                                    gunAtomNode,
                                    privateKeys,
                                    lastTicks,
                                    lastValue,
                                    (fun () ->
                                        addTimestamp
                                            (fun () ->
                                                "[ ||==> setAdapterValue ](j4) invoking debouncedSetAtom. gun inside setAtom passed to debouncedBatchPutFromUi  ")
                                            getDebugInfo

                                        debouncedAdapterSetAtom (false, (lastTicks, lastValue)))
                                )

                        Some setAdapterValue
                    | _ -> failwith $"invalid gun atom node {getDebugInfo ()}"
                | _ -> None),
            (fun getter _setter adapterOptions ->
                match adapterOptions with
                | Atom.AdapterOptions.Gun (_peers, alias) ->
                    let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (Some alias, atomPath))

                    match gunAtomNode with
                    | Some gunAtomNode -> gunAtomNode.off () |> ignore
                    | _ -> ()

                    let getDebugInfo () =
                        $"adapterOptions={adapterOptions} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ <==== unmountFn ](j5) gun unmount ") getDebugInfo

                | _ -> ())
        | Atom.AdapterType.Hub ->
            (fun _getter _setter adapterOptions _setValue ->
                match adapterOptions with
                | Atom.AdapterOptions.Hub (_alias, _hubUrl) ->
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                    Profiling.addTimestamp (fun () -> $"+09B ====> getAtomAdapter hub mount  {getDebugInfo ()}  ")

                    Some
                        (fun (_lastTicks, _lastValue) ->
                            Profiling.addTimestamp
                                (fun () -> $"+09-1B ====> getAtomAdapter hub setAdapterValue  {getDebugInfo ()}  "))
                | _ -> None),
            (fun _getter _setter adapterOptions ->
                match adapterOptions with
                | Atom.AdapterOptions.Hub (_alias, _hubUrl) ->
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                    Profiling.addTimestamp (fun () -> $"+08B <==== getAtomAdapter hub unmount  {getDebugInfo ()}  ")
                | _ -> ()

                )
        | Atom.AdapterType.Memory ->
            (fun _getter _setter adapterOptions _setValue ->
                match adapterOptions with
                | Atom.AdapterOptions.Memory ->
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))

                    let getDebugInfo () =
                        $"adapterOptions={adapterOptions} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ ====> mountFn ](j6) memory mount ") getDebugInfo

                    Some
                        (fun (_lastTicks, _lastValue) ->
                            let getDebugInfo () =
                                $"_lastTicks={_lastTicks} _lastValue={_lastValue} {getDebugInfo ()}"

                            addTimestamp
                                (fun () -> "[ ¨¨ setAdapterValue ](j8) memory inside debouncedPutFromUi setAtom ")
                                getDebugInfo)
                | _ -> None),
            (fun _getter _setter adapterOptions ->
                match adapterOptions with
                | Atom.AdapterOptions.Memory ->
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))

                    let getDebugInfo () =
                        $"adapterOptions={adapterOptions} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ <==== unmountFn ](j7) memory unmount ") getDebugInfo
                | _ -> ())

    //    let inline getCollectionAdapter<'A> storeAtomPath adapterType =
//        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath
//
//        let getDebugInfo adapterOptions =
//            $"atomPath={atomPath} adapterOptions={adapterOptions} adapterType={adapterType}"
//
//        match adapterType with
//        | Atom.AdapterType.Gun ->
//            (fun _getter _setter adapterOptions _setValue ->
//                match adapterOptions with
//                | Atom.AdapterOptions.Gun (_alias, _peers) ->
//                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
//                    Profiling.addTimestamp
//                        (fun () -> $"+11b ----> getCollectionAdapterOptions gun mount {getDebugInfo ()} ")
//                | _ -> ()),
//            (fun _getter _setter adapterOptions ->
//                match adapterOptions with
//                | Atom.AdapterOptions.Gun (_alias, _peers) ->
//                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
//                    Profiling.addTimestamp
//                        (fun () -> $"+10b <---- getCollectionAdapterOptions gun unmount  {getDebugInfo ()}  ")
//                | _ -> ())
//        | Atom.AdapterType.Hub ->
//            (fun _getter _setter adapterOptions _setValue ->
//                match adapterOptions with
//                | Atom.AdapterOptions.Hub (_alias, _hubUrl) ->
//                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
//                    Profiling.addTimestamp
//                        (fun () -> $"+09b ----> getCollectionAdapterOptions hub mount  {getDebugInfo ()}  ")
//                | _ -> ()),
//            (fun _getter _setter adapterOptions ->
//                match adapterOptions with
//                | Atom.AdapterOptions.Hub (_alias, _hubUrl) ->
//                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
//                    Profiling.addTimestamp
//                        (fun () -> $"+08b <---- getCollectionAdapterOptions hub unmount  {getDebugInfo ()}  ")
//                | _ -> ())
//        | Atom.AdapterType.Memory ->
//            (fun _getter _setter adapterOptions _setValue ->
//                match adapterOptions with
//                | Atom.AdapterOptions.Memory ->
//                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
//                    Profiling.addTimestamp
//                        (fun () -> $"+07b  ----> getCollectionAdapterOptions memory mount  {getDebugInfo ()}  ")
//                | _ -> ()),
//            (fun _getter _setter adapterOptions ->
//                match adapterOptions with
//                | Atom.AdapterOptions.Memory ->
//                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
//                    Profiling.addTimestamp
//                        (fun () -> $"+06b <---- getCollectionAdapterOptions memory unmount  {getDebugInfo ()}  ")
//                | _ -> ())



    //    let inline createAdapterAtomMap defaultValue getAdapter getDebugInfo =
//        let subscriptionHandleMap = Dictionary<Atom.AdapterType, bool> ()
//
//        let createAdapterAtom adapterType =
//            Atom.create (AtomType.Atom defaultValue)
//            |> wrapAtomWithState
//                (fun getter ->
//                    let adapterOptions = getAdapterOptions getter adapterType
//
//                    Profiling.addTimestamp
//                        $"+04a * subscribeCollection [ stateFn ] adapterType={adapterType} adapterOptions={Json.encodeWithNull adapterOptions} {getDebugInfo ()}"
//
//                    match adapterOptions with
//                    | Some adapterOptions ->
//                        let mount, unmount = getAdapter adapterType getDebugInfo
//                        // mount adapterOptions
//                        Some (adapterType, adapterOptions, mount, unmount)
//                    | None -> None)
//                (fun getter setter (adapterType, adapterOptions, mount, _) setAtom ->
//                    promise {
//                        //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
//                        Profiling.addTimestamp
//                            $"+03a @@> subscribeCollection mount adapterType={adapterType} adapterOptions={adapterOptions} {getDebugInfo ()}"
//
//                        let setValue value = setAtom value
//
//                        let mounted = subscriptionHandleMap.[adapterType]
//
//                        if not mounted then
//                            mount getter setter adapterOptions setValue
//                            subscriptionHandleMap.[adapterType] <- true
//                    })
//                (fun getter setter (adapterType, adapterOptions, _, unmount) ->
//                    Profiling.addTimestamp
//                        $"+02a <@@ subscribeCollection unmount adapterType={adapterType} adapterOptions={adapterOptions} {getDebugInfo ()} "
//
//                    let mounted = subscriptionHandleMap.[adapterType]
//
//                    if mounted then
//                        unmount getter setter
//                        subscriptionHandleMap.[adapterType] <- false)
//
//        Reflection.unionCases<Atom.AdapterType>
//        |> List.map
//            (fun adapterType ->
//                subscriptionHandleMap.Add (adapterType, false)
//
//                let wrapper = createAdapterAtom adapterType
//                wrapper?init <- defaultValue
//
//                adapterType, wrapper)
//        |> Map.ofList


    //    let inline subscribeCollection<'T when 'T: comparison>
//        (storeRoot: StoreRoot)
//        collection
//        (_onFormat: TicksGuid -> 'T)
//        : AtomConfig<AtomConfig<'T> []> =
//        let storeAtomPath = CollectionAtomPath (storeRoot, collection)
//
//        let getDebugInfo () =
//            $"storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath}"
//
//        Profiling.addTimestamp (fun () -> $"+05a subscribeCollection [ constructor ] {getDebugInfo ()}")
//
//        let atom = createRegisteredAtomWithGroup storeAtomPath (Atom.AdapterType.Memory, [||])
//
//        Atom.Primitives.readSelector
//            (fun getter ->
//                let alias = Atom.get getter Selectors.Gun.alias
//
//                let adapterOptionsList =
//                    Reflection.unionCases<Atom.AdapterType>
//                    |> List.choose
//                        (fun adapterType ->
//                            match getAdapterOptions getter adapterType with
//                            | Some adapterOptions -> Some (adapterType, adapterOptions)
//                            | None -> None)
//
//                let _adapterValues =
//                    adapterOptionsList
//                    |> List.toArray
//                    |> Array.map
//                        (fun (adapterType, _) ->
//                            if
//                                subscriptionAdapterFnMap.ContainsKey (adapterType, storeAtomPath)
//                                |> not
//                            then
//                                subscriptionAdapterFnMap.[(adapterType, storeAtomPath)] <- getCollectionAdapter
//                                atomDefaultValueMap.[storeAtomPath] <- box [||]
//
//                            subscriptionFamily (alias, storeAtomPath, adapterType))
//                    |> Array.map (Atom.get getter)
//
//
//                let ticks, adapterType, value = Atom.get getter atom
//
//                Profiling.addTimestamp
//                    (fun () ->
//                        $"+01a Engine.subscribeCollection wrapper.get() ticks={ticks} adapterType={adapterType} result={Json.encodeWithNull value} {getDebugInfo ()}")
//
//                value)
//        |> Atom.split

    //            storeAtomPath:StoreAtomPath -> adapterType:Atom.AdapterType -> (Getter<obj> -> obj -> Atom.AdapterOptions -> (TicksGuid * 'A -> unit) -> (TicksGuid * 'A -> unit) option) * (Getter<obj> -> obj -> Atom.AdapterOptions -> unit)

    //    type AdapterValue<'A> = AdapterValue of ticks: TicksGuid * value: 'A

    let inline wrapAtomWithAdapter<'A when 'A: equality>
        adapterType
        (getAdapter: Atom.AdapterType
                         -> (Getter<obj> -> Setter<obj> -> Atom.AdapterOptions -> (bool * (TicksGuid * 'A) -> unit) -> (bool * (TicksGuid * 'A) -> unit) option) * (Getter<obj> -> Setter<obj> -> Atom.AdapterOptions -> unit))
        atom
        : AtomConfig<bool * (TicksGuid * 'A)> =
        let mutable setAdapterValue: (bool * (TicksGuid * 'A) -> unit) option = None

        let getDebugInfo () =
            $" adapterType={adapterType} atom={atom} setAdapterValue.IsSome={setAdapterValue.IsSome} "

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp
                (fun () -> $"{nameof FsStore} | Engine.wrapAtomWithAdapter {fn ()} | {getDebugInfo ()}")

        addTimestamp (fun () -> "[ constructor ](f1)") getDebugInfo

        let mutable lastAdapterValue = JS.undefined

        Atom.Primitives.selector
            (fun getter ->
                let result = Atom.get getter atom
                let getDebugInfo () = $"result={result} {getDebugInfo ()}"
                addTimestamp (fun () -> "[ wrapper.get() ](f2)") getDebugInfo
                result)
            (fun _ setter (newFromUi, (newTicks, newValue)) ->
                let getDebugInfo () =
                    $" newTicks={newTicks} newValue={newValue} newFromUi={newFromUi} {getDebugInfo ()}"

                addTimestamp (fun () -> "[ (^^^^1) wrapper.set() ](f3-2) ") getDebugInfo

                Atom.change
                    setter
                    atom
                    (fun (oldFromUi, (_oldTicks, oldValue)) ->
                        let getDebugInfo () =
                            $"_oldTicks={_oldTicks} oldValue={oldValue} oldFromUi={oldFromUi} {getDebugInfo ()}"

                        match setAdapterValue with
                        | Some setAdapterValue when oldValue |> Object.compare newValue |> not ->
                            addTimestamp
                                (fun () -> "[ (^^^^2) wrapper.set() ](f3) triggering new adapter value")
                                getDebugInfo

                            // gunPut
                            setAdapterValue (newFromUi, (newTicks, newValue))
                        | _ ->
                            addTimestamp
                                (fun () -> "[ (^^^^2) wrapper.set() ](f3-1) skipping new adapter assign")
                                getDebugInfo

                        newFromUi, (newTicks, newValue)))
        |> wrapAtomWithState
            (fun getter ->
                let adapterOptions = getAdapterOptions getter adapterType

                let getDebugInfo () =
                    $"adapterOptions={Json.encodeWithNull adapterOptions} {getDebugInfo ()}"

                addTimestamp (fun () -> "[ state.read() ](f5)") getDebugInfo

                match adapterOptions with
                | Some adapterOptions ->
                    let mount, unmount = getAdapter adapterType
                    // mount adapterOptions
                    Some (adapterOptions, mount, unmount)
                | None -> None)
            (fun getter setter (adapterOptions, mount, _) setAtom ->
                promise {
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))

                    let getDebugInfo () =
                        $"setAdapterValue.IsNone={setAdapterValue.IsNone} adapterOptions={Json.encodeWithNull adapterOptions} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ @@> mount ](f6)") getDebugInfo

                    if setAdapterValue.IsNone then
                        setAdapterValue <- mount getter setter adapterOptions setAtom
                })
            (fun getter setter (adapterOptions, _, unmount) ->
                let getDebugInfo () =
                    $"setAdapterValue.IsNone={setAdapterValue.IsNone} adapterOptions={Json.encodeWithNull adapterOptions} {getDebugInfo ()}"

                addTimestamp (fun () -> "[ <@@ unmount ](f7)") getDebugInfo

                if setAdapterValue.IsSome then
                    unmount getter setter adapterOptions
                    setAdapterValue <- None)


    let inline groupValues<'TGroup, 'A when 'TGroup: comparison and 'A: equality> groupMap =
        groupMap
        |> Seq.groupBy (fun (_, (group: 'TGroup, _)) -> group)
        |> Seq.map
            (fun (group, values) ->
                let newItem =
                    values
                    |> Seq.sortByDescending fst
                    |> Seq.map (fun (ticks: TicksGuid, (_, value: 'A)) -> ticks, value)
                    |> Seq.head

                group, newItem)
        |> Seq.sortByDescending fst
        |> Seq.toList


    let groupAtomDefaultValueMap<'TGroup, 'A when 'TGroup: comparison and 'A: equality> =
        Dictionary<Gun.Alias option * StoreAtomPath, TicksGuid * ('TGroup * 'A)> ()

    let groupMapAtom<'TGroup, 'A when 'TGroup: comparison and 'A: equality> =
        Atom.atomFamilyAtom
            (fun (alias: Gun.Alias option, storeAtomPath: StoreAtomPath) ->
                let getDebugInfo () =
                    $"alias={alias} storeAtomPath={StoreAtomPath.AtomPath}"

                let addTimestamp fn getDebugInfo =
                    Profiling.addTimestamp
                        (fun () -> $"{nameof FsStore} | Engine.groupMapAtom {fn ()} | {getDebugInfo ()}")

                addTimestamp (fun () -> "[ constructor ](e3)") getDebugInfo

                groupAtomDefaultValueMap<'TGroup, 'A>.[alias, storeAtomPath]
                |> List.singleton)

    let lastSyncValueByTypeAtom<'TGroup, 'A when 'TGroup: comparison and 'A: equality> =
        Atom.Primitives.readSelectorFamily
            (fun (alias: Gun.Alias option, storeAtomPath: StoreAtomPath) getter ->
                let groupMap = Atom.get getter (groupMapAtom<'TGroup, 'A> (alias, storeAtomPath))

                let getDebugInfo () =
                    $"alias={alias} storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath}"

                let addTimestamp fn getDebugInfo =
                    Profiling.addTimestamp
                        (fun () -> $"{nameof FsStore} | Engine.lastSyncValueByTypeAtom {fn ()} | {getDebugInfo ()}")

                addTimestamp (fun () -> "[ constructor ](e2)") getDebugInfo

                groupMap
                //                |> unbox<(TicksGuid * (obj * obj)) list>
                |> groupValues<'TGroup, 'A>)


    let subscriptionAdapterFnMap<'A when 'A: equality> =
        Dictionary<Atom.AdapterType * StoreAtomPath, StoreAtomPath
            -> Atom.AdapterType
//            -> (Getter<obj> -> Setter<obj> -> Atom.AdapterOptions -> (bool * (TicksGuid * 'A) -> unit) -> (bool * (TicksGuid * 'A) -> unit) option) * (Getter<obj> -> Setter<obj> -> Atom.AdapterOptions -> unit)>
            -> (Getter<obj> -> Setter<obj> -> Atom.AdapterOptions -> _ -> _ option) * (Getter<obj> -> Setter<obj> -> Atom.AdapterOptions -> unit)>
            ()

    let atomDefaultValueMap<'A when 'A: equality> = Dictionary<StoreAtomPath, 'A> ()

    let userAdapterFamily<'A when 'A: equality> =
        Atom.Primitives.atomFamily
            (fun (_alias: Gun.Alias, storeAtomPath, adapterType) ->
                //                let getAdapter = subscriptionAdapterFnMap<'TGroup, 'A>.[(adapterType, storeAtomPath)]
                let defaultValue: 'A = atomDefaultValueMap<'A>.[storeAtomPath]

                let getDebugInfo () =
                    $"_alias={_alias} storeAtomPath={StoreAtomPath.AtomPath} adapterType={adapterType} defaultValue={defaultValue}"

                let addTimestamp fn getDebugInfo =
                    Profiling.addTimestamp
                        (fun () -> $"{nameof FsStore} | Engine.userAdapterFamily {fn ()} | {getDebugInfo ()}")

                addTimestamp (fun () -> "[ constructor ](e1)") getDebugInfo

                let atom = JS.undefined

                Atom.create (AtomType.Atom (false, ((Guid.Empty: TicksGuid), (defaultValue: 'A))))
                //                atom
                |> wrapAtomWithAdapter<'A>
                    adapterType
                    (subscriptionAdapterFnMap<'A>.[(adapterType, storeAtomPath)] storeAtomPath)
                |> Atom.register storeAtomPath)

    let inline subscribeFamilyKey<'TKey, 'A when 'TKey: equality and 'A: equality>
        (_atomFamily: 'TKey -> AtomConfig<'A>)
        =
        let result: AtomConfig<AtomConfig<'TKey> []> = Atom.create (AtomType.Atom [||])
        result

    let inline createRegisteredAtomWithGroup<'TGroup, 'A when 'TGroup: comparison and 'A: equality>
        (storeAtomPath: StoreAtomPath)
        (defaultGroup: 'TGroup, defaultValue: 'A)
        : AtomConfig<('TGroup * (TicksGuid * 'A)) list> =

        let defaultValue = Guid.Empty, (defaultGroup, defaultValue)

        let getDebugInfo () =
            $"atomPath={storeAtomPath |> StoreAtomPath.AtomPath} defaultValue={defaultValue}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp
                (fun () -> $"{nameof FsStore} | Engine.createRegisteredAtomWithGroup {fn ()} | {getDebugInfo ()}")

        addTimestamp (fun () -> "[ constructor ](d1)") getDebugInfo

        let wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    let alias = Atom.get getter Selectors.Gun.alias

                    if groupAtomDefaultValueMap<'TGroup, 'A>.ContainsKey (alias, storeAtomPath)
                       |> not then
                        groupAtomDefaultValueMap<'TGroup, 'A>.[(alias, storeAtomPath)] <- defaultValue

                    let userAtom = lastSyncValueByTypeAtom<'TGroup, 'A> (alias, storeAtomPath)
                    let lastSyncValueByType = Atom.get getter userAtom

                    let result =
                        lastSyncValueByType
                        |> Seq.sortByDescending fst
                        |> Seq.toList

                    let filteredResult =
                        result
                        |> List.filter
                            (fun (_, (group', _)) ->
                                result.Length = 1
                                || group' <> (result |> List.head |> snd |> fst))
                    //                        |> unbox<(TicksGuid * ('TGroup * 'A)) list>

                    let getDebugInfo () =
                        $"alias={alias} userAtom={userAtom} filteredResult={Json.encodeWithNull filteredResult} {getDebugInfo ()} "

                    addTimestamp (fun () -> "[ wrapper.get() ](d2)") getDebugInfo

                    filteredResult)
                (fun getter setter (newValueFn: ('TGroup * (TicksGuid * 'A)) list) ->
                    let alias = Atom.get getter Selectors.Gun.alias

                    if groupAtomDefaultValueMap<'TGroup, 'A>.ContainsKey (alias, storeAtomPath)
                       |> not then
                        groupAtomDefaultValueMap<'TGroup, 'A>.[(alias, storeAtomPath)] <- defaultValue

                    let getDebugInfo () = $"alias={alias} {getDebugInfo ()} "

                    addTimestamp (fun () -> "[ wrapper.set() ](d3)") getDebugInfo

                    Atom.change
                        setter
                        (groupMapAtom (alias, storeAtomPath))
                        (newValueFn
                         |> unbox<(TicksGuid * ('TGroup * 'A)) list -> (TicksGuid * ('TGroup * 'A)) list>))
            |> Atom.register storeAtomPath

        wrapper?init <- defaultValue

        wrapper




    let inline getAdapterValues<'A when 'A: equality> getter storeAtomPath (defaultValue: 'A) =
        let alias = Atom.get getter Selectors.Gun.alias

        let getDebugInfo () =
            $"alias={alias} storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath} defaultValue={defaultValue}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.getAdapterValues {fn ()} | {getDebugInfo ()}")

        Reflection.unionCases<Atom.AdapterType>
        |> List.choose
            (fun adapterType ->
                match alias, getAdapterOptions getter adapterType with
                | Some alias, Some adapterOptions ->
                    if subscriptionAdapterFnMap<'A>.ContainsKey (adapterType, storeAtomPath)
                       |> not then
                        subscriptionAdapterFnMap<'A>.[(adapterType, storeAtomPath)] <- getAdapterSubscription<'A>

                        atomDefaultValueMap<'A>.[storeAtomPath] <- defaultValue

                    let adapterValueAtom = userAdapterFamily<'A> (alias, storeAtomPath, adapterType)
                    //                        |> unbox<AtomConfig<TicksGuid * 'A>>

                    let adapterValue = Atom.get getter adapterValueAtom

                    let getDebugInfo () =
                        $"adapterType={adapterType} adapterOptions={adapterOptions} adapterValue={adapterValue} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ refreshAdapterValues ](a4) returning valid adapter") getDebugInfo

                    Some (adapterType, adapterOptions, adapterValueAtom, adapterValue)
                | _ -> None)

    let inline sync<'A when 'A: equality>
        (adapterValues: (Atom.AdapterType * Atom.AdapterOptions * AtomConfig<bool * (TicksGuid * 'A)> * (bool * (TicksGuid * 'A))) list)
        (atom: AtomConfig<((Atom.AdapterType * bool) * (TicksGuid * 'A)) list>)
        =
        match lastStore with
        | Some (getter, setter) ->
            let getDebugInfo () =
                $"atom={atom} adapterValues={Json.encodeWithNull adapterValues}"

            let addTimestamp fn getDebugInfo =
                Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.sync {fn ()} | {getDebugInfo ()}")

            let localAdapters = Atom.get getter atom
            //                    lastLocalAdapters <- Some localAdapters

            let ((lastAdapterType: Atom.AdapterType), (lastFromUi: bool)), (lastTicks: TicksGuid, lastValue) =
                localAdapters |> List.head

            let getDebugInfo () =
                $" localAdapters={Json.encodeWithNull localAdapters} {getDebugInfo ()} "

            let values =
                adapterValues
                |> List.map
                    (fun (adapterType, _adapterOptions, adapterAtom, (fromUi, (adapterTicks, adapterValue))) ->
                        (fun (fromUi, (ticks, newValue)) ->
                                                    let getDebugInfo () =
                                                        $"adapterType={adapterType} adapterTicks={adapterTicks} adapterValue={adapterValue} ticks={ticks} newValue={newValue} {getDebugInfo ()} "

                                                    addTimestamp (fun () -> "[ (:::::) adapter.write() ](c1) invoking Atom.set") getDebugInfo

                                                    Atom.set setter adapterAtom (fromUi, (ticks, newValue))),
                        (adapterType, fromUi), (adapterTicks, adapterValue))

            //            let _, setLastAdapterAtom, _, _ =
//                values
//                |> List.find (fun (adapterType, _, _, _) -> adapterType = lastAdapterType)
//
            let values =
                values
                |> List.append [
                    (fun (fromUi, (ticks, newValue)) ->
                        addTimestamp
                            (fun () -> "[ (:::::) lastAdapter.write() ](c2) invoking Atom.change ")
                            getDebugInfo

                        Atom.change
                            setter
                            atom
                            (fun oldValue ->
                                let getDebugInfo () =
                                    $"lastAdapterType={lastAdapterType} oldValue={oldValue} ticks={ticks} newValue={newValue} {getDebugInfo ()} "

                                addTimestamp
                                    (fun () -> "[ (:::::) lastAdapter.write() ](c2) inside Atom.change ")
                                    getDebugInfo

                                ((lastAdapterType, fromUi), (ticks, newValue))
                                :: (oldValue
                                    |> List.filter (fun ((adapterType, _), _) -> adapterType <> lastAdapterType)))),
                    (lastAdapterType, true), (lastTicks, lastValue)
                   ]
                |> List.sortByDescending (fun (_,_,(ticks, _)) -> ticks)

            let valuesfmt =
                Json.encodeWithNull (
                    values
                    |> List.map (fun (_setAdapterAtom,(adapterType, fromUi), (ticks, value)) -> "adapterType", adapterType, "fromUi",fromUi, "ticks",ticks, "value",value)
                )

            let getDebugInfo () =
                $" {getDebugInfo ()} values={valuesfmt}"

            let lastAdapterSetAtom,(lastAdapterType, lastFromUi), (lastTicks, lastValue)) = values.Head


            values
            |> List.skip 1
            |> List.filter (fun (_,_, (_, value)) -> value |> Object.compare lastValue |> not)
            |> List.map
                (fun (setAdapterAtom,(adapterType, fromUi), (ticks, value)) ->
                    promise {
                        let getDebugInfo () =
                            $" adapterFromUi={fromUi} lastFromUi={lastFromUi} adapterType={adapterType} lastAdapterType={lastAdapterType} lastTicks={lastTicks} ticks={ticks} lastValue={lastValue} value={value} {getDebugInfo ()} "

                        if lastTicks = ticks then
                            addTimestamp
                                (fun () -> "[ (%%%%) invalidAdapter.write() ](c3) same ticks. skipping")
                                getDebugInfo
                        elif lastTicks > ticks then
                            // set adapter value from local atom

                            addTimestamp (fun () -> "[ (%%%%) invalidAdapter.write() ](c4)") getDebugInfo
                            setAdapterAtom (lastFromUi, (lastTicks, lastValue))
                        else
                            addTimestamp
                                (fun () ->
                                    "[ invalidAdapter.write() ](c5) (%%%%) assigning current atom. adapter is newer (probably wont invoke)")
                                getDebugInfo

                            // set local atom from adapter value
                            lastAdapterSetAtom (fromUi,(ticks, value))

                        ()
                    })
            |> List.toArray
            |> Promise.all
            |> Promise.ignore
            |> Promise.start
        | _ -> ()


    let inline createRegisteredAtomWithSubscription<'A when 'A: equality>
        storeAtomPath
        (defaultValue: 'A)
        : AtomConfig<'A> =
        let localAdaptersAtom =
            createRegisteredAtomWithGroup<Atom.AdapterType * bool, 'A> storeAtomPath ((Atom.AdapterType.Memory, false), defaultValue)

        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

        let getDebugInfo () =
            $"localAdaptersAtom={localAdaptersAtom} atomPath={atomPath} defaultValue={defaultValue}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp
                (fun () ->
                    $"{nameof FsStore} | Engine.createRegisteredAtomWithSubscription {fn ()} | {getDebugInfo ()}")

        addTimestamp (fun () -> "[ constructor ](a1)") getDebugInfo

        let debouncedSync =
            Js.debounce
                (fun adapterValues ->
                    addTimestamp (fun () -> "[ debouncedSync ](a2)") getDebugInfo
                    sync<'A> adapterValues localAdaptersAtom)
                0

        //        let newSync =
//            debouncedSync
//                localAdaptersAtom
//                (fun getter adapterType setAtom lastTicks lastValue ->
//                    match adapterType with
//                    | Atom.AdapterType.Gun ->
//                        let alias = Atom.get getter Selectors.Gun.alias
//                        let privateKeys = Atom.get getter Selectors.Gun.privateKeys
//                        let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
//
//                        match privateKeys, gunAtomNode with
//                        | Some privateKeys, Some gunAtomNode ->
//                            debouncedBatchPutFromUi (
//                                gunAtomNode,
//                                privateKeys,
//                                (fun (_ticks, value) ->
//                                    addTimestamp
//                                        (fun () ->
//                                            $" [ sync ]¨¨ inside debouncedPutFromUi setAtom. _ticks={_ticks} value={value} lastTicks={lastTicks} lastValue={lastValue} ")
//                                        getDebugInfo
//
//                                    setAtom (lastTicks, value)),
//                                lastTicks,
//                                lastValue
//                            )
//                        | _ -> addTimestamp (fun () -> $" [ sync ] gun. skipped. no keys or node. ") getDebugInfo
//                    | _ ->
//                        addTimestamp (fun () -> $" [ sync ] assigning adapter from last. ") getDebugInfo
//
//                        setAtom (lastTicks, lastValue))

        let inline refreshAdapterValues getter =
            addTimestamp (fun () -> "[ refreshAdapterValues ](a3)") getDebugInfo
            let adapterValues = getAdapterValues<'A> getter storeAtomPath defaultValue
            debouncedSync adapterValues

        let rec wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    refreshAdapterValues getter
                    let localAdapters = Atom.get getter localAdaptersAtom
                    let _ticks, (_adapterType, value) = localAdapters |> List.head


                    let getDebugInfo () =
                        $"_ticks={_ticks} _adapterType={_adapterType} value={value} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ wrapper.get() ](a5)") getDebugInfo

                    value)
                (fun getter setter (newValue: 'A) ->
                    refreshAdapterValues getter

                    Atom.change
                        setter
                        localAdaptersAtom
                        (fun localAdapters ->
                            let newItem = (Atom.AdapterType.Memory, true), (Guid.newTicksGuid (), newValue)

                            let getDebugInfo () = $"newItem={newItem} {getDebugInfo ()}"

                            addTimestamp (fun () -> "[ <&¨<->¨&> wrapper.set() ](a6)") getDebugInfo

                            localAdapters
                            |> List.filter (fun ((adapterType, _), _) -> adapterType <> Atom.AdapterType.Memory)
                            |> List.append (newItem |> List.singleton)))

        wrapper?init <- defaultValue

        wrapper |> Atom.register storeAtomPath

    let inline bindAtom<'A when 'A: equality> atom1 atom2 =
        let mutable lastSetAtom: ('A option -> unit) option = None
        let mutable lastValue = None

        let storeAtomPath = Atom.query (AtomReference.Atom atom1)

        let getDebugInfo () =
            $"atom1={atom1} atom2={atom2} atomPath={storeAtomPath |> StoreAtomPath.AtomPath} lastValue={lastValue}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.bindAtom {fn ()} | {getDebugInfo ()}")

        addTimestamp (fun () -> "[ constructor ](b1)") getDebugInfo

        let rec wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    match atom1.init, atom2.init with
                    | default1, default2 when default1 <> unbox null && default2 <> unbox null ->
                        match Atom.get getter atom1, Atom.get getter atom2 with
                        | value1, value2 when
                            value1 |> Object.compare default1.Value
                            && (value2 |> Object.compare default2.Value
                                || lastValue.IsNone
                                || (Atom.get getter Selectors.Gun.alias).IsNone)
                            ->
                            let getDebugInfo () =
                                $"value1={value1} value2={value2} {getDebugInfo ()}"

                            addTimestamp (fun () -> "[ wrapper.get() ](b2) choosing value2") getDebugInfo
                            value2
                        | value1, value2 ->
                            let getDebugInfo () =
                                $"value1={value1} value2={value2} {getDebugInfo ()}"

                            match lastSetAtom with
                            | Some lastSetAtom when
                                lastValue.IsNone
                                || lastValue |> Object.compare (Some value1) |> not
                                ->
                                addTimestamp
                                    (fun () -> "[ wrapper.get() ](b3) different. triggering additional")
                                    getDebugInfo

                                lastValue <- Some value1
                                lastSetAtom (Some value1)
                            | _ -> ()

                            addTimestamp (fun () -> "[ wrapper.get() ](b4) choosing value1") getDebugInfo

                            value1
                    | _ -> failwith $"bindAtom. atoms without default value. {getDebugInfo ()}")
                (fun _get setter newValue ->
                    let getDebugInfo () =
                        $"newValue={newValue} {getDebugInfo ()}"

                    if lastValue.IsNone
                       || lastValue |> Object.compare (Some newValue) |> not then
                        lastValue <- Some newValue
                        Atom.set setter atom1 newValue

                        addTimestamp (fun () -> "[ wrapper.set() ](b5) setting atom1 and atom2") getDebugInfo
                    else
                        addTimestamp (fun () -> "[ wrapper.set() ](b6) setting atom2 only") getDebugInfo

                    Atom.set setter atom2 newValue)

        wrapper?init <- atom1.init

        wrapper |> Atom.register storeAtomPath


    let inline createRegisteredAtomWithSubscriptionStorage<'A when 'A: equality> storeAtomPath (defaultValue: 'A) =
        //        let storageAtom = Atom.createRegisteredWithStorage storeAtomPath (Guid.Empty, defaultValue)
        let storageAtom = Atom.createRegisteredWithStorage<'A> storeAtomPath defaultValue
        let syncAtom = createRegisteredAtomWithSubscription<'A> storeAtomPath defaultValue
        bindAtom<'A> syncAtom storageAtom

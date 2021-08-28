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

            Profiling.addTimestamp (fun () -> $">25f Engine.consumeCommands {getDebugInfo ()}")

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
            $" | atom={atom} mounted={mounted} storeAtomPath={storeAtomPath |> Option.map StoreAtomPath.AtomPath} lastState={Json.encodeWithNull lastState} "

        Profiling.addTimestamp (fun () -> $"+24e Engine.wrapAtomWithState [ constructor ] {getDebugInfo ()}")

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
                    Profiling.addTimestamp
                        (fun () -> $"+23e Engine.wrapAtomWithState newMount(). invoking mount {getDebugInfo ()}")

                    mounted <- true
                    do! mount getter setter state lastSetAtom.Value
                | None ->
                    Profiling.addTimestamp
                        (fun () -> $"+22e Engine.wrapAtomWithState newMount(). skipping, no state. {getDebugInfo ()}")
            }

        let newUnmount () =
            if mounted then
                match getState () with
                | Some (getter, setter, state) ->
                    Profiling.addTimestamp
                        (fun () -> $"+21e Engine.wrapAtomWithState newUnmount(). invoking unmount {getDebugInfo ()}")

                    mounted <- false
                    lastState <- None
                    unmount getter setter state

                //                    JS.setTimeout
//                        (fun () ->
//                            Profiling.addTimestamp (fun () -> $"Engine.wrapAtom onUnmount() clearing lastState {getDebugInfo ()}"
//                            lastState <- None
//                            )
//                        0
//                    |> ignore
                | None ->
                    Profiling.addTimestamp
                        (fun () ->
                            $"+20e Engine.wrapAtomWithState newUnmount(). skipping, no state. (should unmount here???) {getDebugInfo ()}")

        let refreshInternalState getter =
            if lastStore.IsNone then lastStore <- Atom.get getter Selectors.store

            let logger = Atom.get getter Selectors.logger
            Logger.State.lastLogger <- logger

            let newState = stateFn getter

            Profiling.addTimestamp
                (fun () ->
                    $"+19e Engine.wrapAtomWithState refreshInternalState (get or set). newState={Json.encodeWithNull newState}. will mount or unmount. {getDebugInfo ()}")

            if lastSetAtom.IsNone then
                Profiling.addTimestamp
                    (fun () ->
                        $"+19-1e X Engine.wrapAtomWithState refreshInternalState. skipping mount/unmount. lastSetAtom not found {getDebugInfo ()}")
            else
                match newState with
                | Some _ ->
                    lastState <- newState
                    newMount () |> Promise.start
                | None -> newUnmount ()

        let wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    refreshInternalState getter

                    let result = Atom.get getter atom

                    Profiling.addTimestamp (fun () -> $"+18e Engine.wrapAtomWithState wrapper.get() {getDebugInfo ()}")

                    result)
                (fun getter setter newValue ->
                    refreshInternalState getter

                    Profiling.addTimestamp
                        (fun () ->
                            $"+17e Engine.wrapAtomWithState wrapper.set()  {getDebugInfo ()} newValue={newValue}")

                    Atom.set setter atom newValue)
            |> Atom.addSubscription
                true
                (fun setAtom ->
                    lastSetAtom <- Some setAtom
                    newMount ())
                (fun () -> newUnmount ())

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

        let cache = Atom.Primitives.atom defaultValue

        let wrapper =
            atom
            |> wrapAtom
                (fun getter setter _setAtom ->
                    promise {
                        let logger = Logger.State.lastLogger
                        Profiling.addTimestamp (fun () -> $"+16d Engine.wrapAtomWithInterval. mount. {getDebugInfo ()}")

                        let fn () =
                            logger.Trace
                                (fun () -> $"Engine.wrapAtomWithInterval. mount interval fn. {getDebugInfo ()}")

                            if intervalHandle >= 0 then
                                let atomValue = Atom.get getter atom

                                if Some atomValue |> Object.compare lastValue |> not then
                                    Profiling.addTimestamp
                                        (fun () ->
                                            $"+15d Engine.wrapAtomWithInterval. mount interval fn. atomValue={atomValue}. {getDebugInfo ()}")

                                    Atom.set setter cache atomValue
                                    lastValue <- Some atomValue

                        if intervalHandle = -1 then fn ()
                        intervalHandle <- JS.setInterval fn interval
                    })
                (fun _getter _setter ->
                    //                let logger = Logger.State.lastLogger
                    Profiling.addTimestamp (fun () -> $"+14d Engine.wrapAtomWithInterval unmount() {getDebugInfo ()}")

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

    let inline batchPutFromUi (gunAtomNode, privateKeys, setAtom, ticks, newValue) =
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

                        Profiling.addTimestamp
                            (fun () ->
                                $"{nameof FsStore} | Engine.batchPutFromUi. gun. newValue={newValue} ticks={ticks} putResult={putResult}. ")

                        if putResult then setAtom (ticks, newValue)
                        ()
                    })
            )
        )

    let inline getAdapterSubscription<'A> storeAtomPath adapterType =
        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

        let getDebugInfo () =
            $"atomPath={atomPath} adapterType={adapterType}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp (fun () -> $"Engine.getAdapterSubscription {fn ()} {getDebugInfo ()} ")

        match adapterType with
        | Atom.AdapterType.Gun ->
            (fun getter _setter adapterOptions adapterSetAtom ->
                match adapterOptions with
                | Atom.AdapterOptions.Gun (_peers, alias) ->
                    let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (Some alias, atomPath))
                    let privateKeys = Atom.get getter Selectors.Gun.privateKeys

                    let getDebugInfo () =
                        $"alias={alias} gunAtomNode={gunAtomNode} privateKeys={privateKeys} _peers={_peers} adapterOptions={adapterOptions}"

                    Profiling.addTimestamp (fun () -> $"+11B ====> getAtomAdapter gun mount {getDebugInfo ()} ")

                    match gunAtomNode, privateKeys with
                    | Some gunAtomNode, Some privateKeys ->
                        addTimestamp (fun () -> $"+11.1B ||||||||| gun will batch subscribe.  ") getDebugInfo

                        let debouncedSetAtom =
                            Js.debounce
                                (fun value ->
                                    addTimestamp
                                        (fun () -> $"+11.1B ********> debounced gun on subscribe data. value={value}  ")
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

                                        debouncedSetAtom (newValue |> Option.defaultValue (unbox null))
                                    with
                                    | ex ->
                                        Logger.logError
                                            (fun () ->
                                                $"Engine.getAtomAdapter. gun subscribe data error. ex={ex.Message} gunValue={gunValue} subscriptionTicks={subscriptionTicks} {getDebugInfo ()}")

                                        Logger.consoleError [| ex |]
                                })

                        let debouncedBatchPutFromUi = Js.debounce batchPutFromUi 0

                        let setAdapterValue (lastTicks, lastValue) =
                            debouncedBatchPutFromUi (
                                gunAtomNode,
                                privateKeys,
                                (fun (_ticks, value) ->
                                    addTimestamp
                                        (fun () ->
                                            $" [ sync ]¨¨ inside debouncedPutFromUi setAtom. _ticks={_ticks} value={value} lastTicks={lastTicks} lastValue={lastValue} ")
                                        getDebugInfo

                                    debouncedSetAtom (lastTicks, value)),
                                lastTicks,
                                lastValue
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

                    Profiling.addTimestamp (fun () -> $"+10B <==== getAtomAdapter gun unmount  {getDebugInfo ()}  ")
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
                    Profiling.addTimestamp (fun () -> $"+07B  ====> getAtomAdapter memory mount  {getDebugInfo ()}  ")

                    Some
                        (fun (_lastTicks, _lastValue) ->
                            Profiling.addTimestamp
                                (fun () -> $"+09-1B ====> getAtomAdapter hub setAdapterValue  {getDebugInfo ()}  "))
                | _ -> None),
            (fun _getter _setter adapterOptions ->
                match adapterOptions with
                | Atom.AdapterOptions.Memory ->
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                    Profiling.addTimestamp (fun () -> $"+06B <==== getAtomAdapter memory unmount  {getDebugInfo ()}  ")
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
    let inline wrapAtomWithAdapter<'A> adapterType getAdapter atom : AtomConfig<TicksGuid * 'A> =
        let mutable setAdapterValue: (TicksGuid * 'A -> unit) option = None

        let getDebugInfo () =
            $" adapterType={adapterType} atom={atom} setAdapterValue.IsSome={setAdapterValue.IsSome} "


        Atom.Primitives.selector
            (fun getter ->
                let result = Atom.get getter atom
                result)
            (fun _ setter (newTicks, newValue) ->
                Atom.change
                    setter
                    atom
                    (fun (_oldTicks, oldValue) ->
                        match setAdapterValue with
                        | Some setAdapterValue when oldValue |> Object.compare newValue |> not ->
                            setAdapterValue (newTicks, newValue)
                        | _ -> ()

                        newTicks, newValue))
        |> wrapAtomWithState
            (fun getter ->
                let adapterOptions = getAdapterOptions getter adapterType

                Profiling.addTimestamp
                    (fun () ->
                        $"+04a * Engine.wrapAtomWithAdapter [ state() ] adapterOptions={Json.encodeWithNull adapterOptions} {getDebugInfo ()}")

                match adapterOptions with
                | Some adapterOptions ->
                    let mount, unmount = getAdapter adapterType
                    // mount adapterOptions
                    Some (adapterOptions, mount, unmount)
                | None -> None)
            (fun getter setter (adapterOptions, mount, _) setAtom ->
                promise {
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                    Profiling.addTimestamp
                        (fun () ->
                            $"+03a @@> Engine.wrapAtomWithAdapter mount adapterOptions={adapterOptions} {getDebugInfo ()}")

                    if setAdapterValue.IsNone then
                        setAdapterValue <- mount getter setter adapterOptions setAtom
                })
            (fun getter setter (adapterOptions, _, unmount) ->
                Profiling.addTimestamp
                    (fun () ->
                        $"+02a <@@ Engine.wrapAtomWithAdapter unmount adapterOptions={adapterOptions} {getDebugInfo ()} ")

                if setAdapterValue.IsSome then
                    unmount getter setter adapterOptions
                    setAdapterValue <- None)


    let inline groupValues groupMap =
        groupMap
        |> Seq.groupBy (fun (_, (group: 'TGroup, _)) -> group)
        |> Seq.map
            (fun (group, v) ->
                let ticks, value =
                    v
                    |> Seq.sortByDescending fst
                    |> Seq.map (fun (ticks: TicksGuid, (_, value: 'A)) -> ticks, value)
                    |> Seq.head

                ticks, (group, value))
        |> Seq.toList


    let groupAtomDefaultValueMap = Dictionary<Gun.Alias option * StoreAtomPath, obj> ()

    let groupMapAtom =
        Atom.atomFamilyAtom
            (fun (alias: Gun.Alias option, storeAtomPath: StoreAtomPath) ->
                groupAtomDefaultValueMap.[alias, storeAtomPath]
                |> List.singleton)

    let lastSyncValueByTypeAtom =
        Atom.Primitives.readSelectorFamily
            (fun (alias: Gun.Alias option, storeAtomPath: StoreAtomPath) getter ->
                let groupMap = Atom.get getter (groupMapAtom (alias, storeAtomPath))

                groupMap
                |> unbox<(TicksGuid * (obj * obj)) list>
                |> groupValues)


    let subscriptionAdapterFnMap =
        Dictionary<Atom.AdapterType * StoreAtomPath, StoreAtomPath
            -> Atom.AdapterType
            -> (Getter<obj> -> (AtomConfig<obj> -> obj -> unit) -> Atom.AdapterOptions -> (TicksGuid * obj -> unit) -> (TicksGuid * obj -> unit) option) * (Getter<obj> -> (AtomConfig<obj> -> obj -> unit) -> Atom.AdapterOptions -> unit)>
            ()

    let atomDefaultValueMap = Dictionary<StoreAtomPath, obj> ()

    let userAdapterFamily =
        Atom.Primitives.atomFamily
            (fun (_alias, storeAtomPath, adapterType) ->
                let getAdapter = subscriptionAdapterFnMap.[(adapterType, storeAtomPath)]
                let defaultValue = atomDefaultValueMap.[storeAtomPath]

                Atom.create (AtomType.Atom (Guid.Empty, defaultValue))
                |> wrapAtomWithAdapter adapterType (getAdapter storeAtomPath)
                |> Atom.register storeAtomPath)

    let inline subscribeFamilyKey<'TKey, 'A> (_atomFamily: 'TKey -> AtomConfig<'A>) =
        let result: AtomConfig<AtomConfig<'TKey> []> = Atom.create (AtomType.Atom [||])
        result

    let inline createRegisteredAtomWithGroup
        (storeAtomPath: StoreAtomPath)
        (defaultGroup: 'TGroup, defaultValue: 'A)
        : AtomConfig<(TicksGuid * ('TGroup * 'A)) list> =

        let defaultValue = Guid.Empty, (defaultGroup, defaultValue)

        let getDebugInfo () =
            $"storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath} defaultValue={defaultValue}"

        Profiling.addTimestamp
            (fun () -> $"+13.1c Engine.createRegisteredAtomWithGroup [ constructor ] {getDebugInfo ()}")

        let wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    let alias = Atom.get getter Selectors.Gun.alias

                    if
                        groupAtomDefaultValueMap.ContainsKey (alias, storeAtomPath)
                        |> not
                    then
                        groupAtomDefaultValueMap.[(alias, storeAtomPath)] <- defaultValue

                    let userAtom = lastSyncValueByTypeAtom (alias, storeAtomPath)
                    let lastSyncValueByType = Atom.get getter userAtom

                    let result =
                        lastSyncValueByType
                        |> Seq.sortByDescending fst
                        |> Seq.toList

                    Profiling.addTimestamp
                        (fun () ->
                            $"+13c Engine.createRegisteredAtomWithGroup wrapper.get()  alias={alias} result={result} {getDebugInfo ()}")

                    result
                    |> List.filter
                        (fun (_, (group', _)) ->
                            result.Length = 1
                            || group' <> (result |> List.head |> snd |> fst))
                    |> unbox<(TicksGuid * ('TGroup * 'A)) list>)
                (fun getter setter newValueFn ->

                    let alias = Atom.get getter Selectors.Gun.alias

                    if
                        groupAtomDefaultValueMap.ContainsKey (alias, storeAtomPath)
                        |> not
                    then
                        groupAtomDefaultValueMap.[(alias, storeAtomPath)] <- defaultValue

                    Profiling.addTimestamp
                        (fun () ->
                            $"+12c Engine.createRegisteredAtomWithGroup wrapper.set() alias={alias} {getDebugInfo ()}  ")

                    Atom.change setter (groupMapAtom (alias, storeAtomPath)) (unbox newValueFn))
            |> Atom.register storeAtomPath

        wrapper?init <- defaultValue

        wrapper


    let inline sync atom lastAdapterValues =
        let syncTimestamp fn getDebugInfo =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.sync {fn ()} {getDebugInfo ()}")

        let getDebugInfo () = $" "

        match lastStore with
        | Some (getter, setter) ->
            let localAdapters = Atom.get getter atom
            //                    lastLocalAdapters <- Some localAdapters

            let (lastTicks: TicksGuid, (lastAdapterType: Atom.AdapterType, lastValue)) = localAdapters |> List.head

            let getDebugInfo () =
                $" localAdapters={Json.encodeWithNull localAdapters} {getDebugInfo ()} "

            let values =
                lastAdapterValues
                |> List.map
                    (fun (adapterType, _adapterOptions, adapterAtom, (adapterTicks, adapterValue)) ->
                        adapterType,
                        (fun (ticks, newValue) ->
                            syncTimestamp
                                (fun () ->
                                    $" [ sync ] gun. adapter set. on set() adapterType={adapterType} adapterTicks={adapterTicks} adapterValue={adapterValue} ticks={ticks} newValue={newValue}  ")
                                getDebugInfo

                            Atom.set setter adapterAtom (ticks, newValue)),
                        adapterTicks,
                        adapterValue)
                |> List.append [
                    lastAdapterType,
                    (fun (ticks, newValue) ->
                        Atom.change
                            setter
                            atom
                            (fun oldValue ->
                                syncTimestamp
                                    (fun () ->
                                        $" [ sync ] gun. current adapter set ({lastAdapterType}). on set() oldValue={oldValue} ticks={ticks} newValue={newValue}  ")
                                    getDebugInfo

                                (ticks, (lastAdapterType, newValue)) :: oldValue)),
                    lastTicks,
                    lastValue
                   ]
                |> List.sortByDescending (fun (_, _, ticks, _) -> ticks)

            let valuesfmt =
                Json.encodeWithNull (
                    values
                    |> List.map (fun (adapterType, _, ticks, value) -> adapterType, ticks, value)
                )

            let getDebugInfo () =
                $" {getDebugInfo ()} values={valuesfmt}"

            syncTimestamp (fun () -> $" |[ sync ]|  ") getDebugInfo

            let lastAdapterType, lastSetAtom, lastTicks, lastValue = values.Head

            values
            |> List.skip 1
            |> List.filter
                (fun (adapterType, _, _, value) ->
                    adapterType <> lastAdapterType
                    && value |> Object.compare lastValue |> not)
            |> List.map
                (fun (adapterType, setAtom, ticks, value) ->
                    promise {
                        let getDebugInfo () =
                            $" adapterType={adapterType} lastAdapterType={lastAdapterType} lastTicks={lastTicks} ticks={ticks} lastValue={lastValue} value={value} {getDebugInfo ()} "

                        if lastTicks = ticks then
                            syncTimestamp (fun () -> $" [ sync ] same ticks. skipping.  ") getDebugInfo
                        elif lastTicks > ticks then
                            // set adapter value from local atom

                            setAtom (lastTicks, lastValue)
                        else
                            syncTimestamp
                                (fun () -> $" [ sync ] assigning current atom. adapter is newer ")
                                getDebugInfo

                            // set local atom from adapter value
                            lastSetAtom (ticks, value)

                        ()
                    })
            |> List.toArray
            |> Promise.all
            |> Promise.ignore
            |> Promise.start
        | _ -> ()



    let inline createRegisteredAtomWithSubscription storeAtomPath (defaultValue: 'A) : AtomConfig<'A> =
        let localAdaptersAtom = createRegisteredAtomWithGroup storeAtomPath (Atom.AdapterType.Memory, defaultValue)

        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

        let mutable lastAdapterValues: (Atom.AdapterType * Atom.AdapterOptions * AtomConfig<TicksGuid * 'A> * (TicksGuid * 'A)) list =
            []

        let getDebugInfo () =
            $"localAdaptersAtom={localAdaptersAtom} atomPath={atomPath} lastAdapterValues={Json.encodeWithNull lastAdapterValues} defaultValue={defaultValue}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp
                (fun () -> $"{nameof FsStore} | Engine.createRegisteredAtomWithSubscription {fn ()} {getDebugInfo ()}")

        addTimestamp (fun () -> $" [ constructor ] ") getDebugInfo

        let debouncedSync = Js.debounce (fun () -> sync localAdaptersAtom lastAdapterValues) 0

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

        let refreshAdapterValues getter =
            let alias = Atom.get getter Selectors.Gun.alias

            let result =
                Reflection.unionCases<Atom.AdapterType>
                |> List.choose
                    (fun adapterType ->
                        match getAdapterOptions getter adapterType with
                        | Some adapterOptions ->
                            if
                                subscriptionAdapterFnMap.ContainsKey (adapterType, storeAtomPath)
                                |> not
                            then
                                subscriptionAdapterFnMap.[(adapterType, storeAtomPath)] <-
                                    unbox getAdapterSubscription<'A>

                                atomDefaultValueMap.[storeAtomPath] <- defaultValue

                            let adapterValueAtom =
                                userAdapterFamily (alias, storeAtomPath, adapterType)
                                |> unbox<AtomConfig<TicksGuid * 'A>>

                            let adapterValue = Atom.get getter adapterValueAtom

                            Some (adapterType, adapterOptions, adapterValueAtom, adapterValue)
                        | None -> None)

            lastAdapterValues <- result

            debouncedSync ()

        let rec wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    refreshAdapterValues getter
                    let localAdapters = Atom.get getter localAdaptersAtom
                    let _ticks, (_adapterType, value) = localAdapters |> List.head

                    Profiling.addTimestamp
                        (fun () ->
                            $"+01a Engine.createRegisteredAtomWithSubscription wrapper.get() {getDebugInfo ()} localAdapters={Json.encodeWithNull localAdapters}  ")

                    value)
                (fun getter setter newValue ->
                    refreshAdapterValues getter

                    Atom.change
                        setter
                        localAdaptersAtom
                        (fun localAdapters ->
                            Profiling.addTimestamp
                                (fun () ->
                                    $"+00a Engine.createRegisteredAtomWithSubscription wrapper.set() {getDebugInfo ()} localAdapters={Json.encodeWithNull localAdapters}   ")

                            localAdapters
                            |> List.filter (fun (_, (adapterType, _)) -> adapterType <> Atom.AdapterType.Memory)
                            |> List.append [
                                Guid.newTicksGuid (), (Atom.AdapterType.Memory, newValue)
                               ]))

        wrapper?init <- defaultValue

        wrapper |> Atom.register storeAtomPath

    let inline bindAtom atom1 atom2 =
        let mutable lastSetAtom: ('A option -> unit) option = None
        let mutable lastValue = None

        let storeAtomPath = Atom.query (AtomReference.Atom atom1)

        let rec wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    match atom1.init, atom2.init with
                    | default1, default2 when default1 <> unbox null && default2 <> unbox null ->
                        match Atom.get getter atom1, Atom.get getter atom2 with
                        | value1, value2 when
                            value1 |> Object.compare default1.Value
                            && (value2 |> Object.compare default2.Value
                                || (Atom.get getter Selectors.Gun.alias).IsNone
                                || lastValue.IsNone)
                            ->

                            Profiling.addTimestamp (fun () -> "Engine.bindAtom. get(). choosing value2")
                            value2
                        | value1, _ ->
                            match lastSetAtom with
                            | Some lastSetAtom when
                                lastValue.IsNone
                                || lastValue |> Object.compare (Some value1) |> not
                                ->
                                lastValue <- Some value1
                                lastSetAtom (Some value1)
                            | _ -> ()

                            Profiling.addTimestamp (fun () -> "Engine.bindAtom. get(). choosing value1")
                            value1
                    | _ -> failwith $"bindAtom. atoms without default value. atom1={atom1} atom2={atom2}")
                (fun _get setter newValue ->
                    if lastValue.IsNone
                       || lastValue |> Object.compare (Some newValue) |> not then
                        lastValue <- Some newValue
                        Atom.set setter atom1 newValue

                        Profiling.addTimestamp (fun () -> "Engine.bindAtom. set(). setting value1")
                    else
                        Profiling.addTimestamp (fun () -> "Engine.bindAtom. set(). setting value2 only")

                    Atom.set setter atom2 newValue)

        wrapper?init <- atom1.init

        wrapper |> Atom.register storeAtomPath


    let inline createRegisteredAtomWithSubscriptionStorage storeAtomPath (defaultValue: 'A) =
        //        let storageAtom = Atom.createRegisteredWithStorage storeAtomPath (Guid.Empty, defaultValue)
        let storageAtom = Atom.createRegisteredWithStorage storeAtomPath defaultValue
        let syncAtom = createRegisteredAtomWithSubscription storeAtomPath defaultValue
        bindAtom syncAtom storageAtom

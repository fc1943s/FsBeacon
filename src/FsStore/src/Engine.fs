namespace FsStore

open Fable.Core.JsInterop
open Fable.Core
open FsCore.BaseModel
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

            Profiling.addTimestamp $">25f Engine.consumeCommands {getDebugInfo ()}"

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

        Profiling.addTimestamp $"+24e Engine.wrapAtomWithState [ constructor ] {getDebugInfo ()}"

        let mutable lastSetAtom = fun _ -> failwith "invalid lastSetAtom"

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
                    Profiling.addTimestamp $"+23e Engine.wrapAtomWithState newMount(). invoking mount {getDebugInfo ()}"
                    mounted <- true
                    do! mount getter setter state lastSetAtom
                | None ->
                    Profiling.addTimestamp
                        $"+22e Engine.wrapAtomWithState newMount(). skipping, no state. {getDebugInfo ()}"
            }

        let newUnmount () =
            if mounted then
                match getState () with
                | Some (getter, setter, state) ->
                    Profiling.addTimestamp
                        $"+21e Engine.wrapAtomWithState newUnmount(). invoking unmount {getDebugInfo ()}"

                    mounted <- false
                    lastState <- None
                    unmount getter setter state

                //                    JS.setTimeout
//                        (fun () ->
//                            Profiling.addTimestamp $"Engine.wrapAtom onUnmount() clearing lastState {getDebugInfo ()}"
//                            lastState <- None
//                            )
//                        0
//                    |> ignore
                | None ->
                    Profiling.addTimestamp
                        $"+20e Engine.wrapAtomWithState newUnmount(). skipping, no state. (should unmount here???) {getDebugInfo ()}"

        let refreshInternalState getter =
            if lastStore.IsNone then lastStore <- Atom.get getter Selectors.store

            let logger = Atom.get getter Selectors.logger
            Logger.State.lastLogger <- logger

            let newState = stateFn getter

            Profiling.addTimestamp
                $"+19e Engine.wrapAtomWithState refreshInternalState (get or set). newState={Json.encodeWithNull newState}. will mount or unmount. {getDebugInfo ()}"

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

                    Profiling.addTimestamp $"+18e Engine.wrapAtomWithState wrapper.get() {getDebugInfo ()}"

                    result)
                (fun getter setter newValue ->
                    refreshInternalState getter

                    Profiling.addTimestamp
                        $"+17e Engine.wrapAtomWithState wrapper.set()  {getDebugInfo ()} newValue={newValue}"

                    Atom.set setter atom newValue)
            |> Atom.addSubscription
                true
                (fun setAtom ->
                    lastSetAtom <- setAtom
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
                        Profiling.addTimestamp $"+16d Engine.wrapAtomWithInterval. mount. {getDebugInfo ()}"

                        let fn () =
                            logger.Trace
                                (fun () -> $"Engine.wrapAtomWithInterval. mount interval fn. {getDebugInfo ()}")

                            if intervalHandle >= 0 then
                                let atomValue = Atom.get getter atom

                                if Some atomValue |> Object.compare lastValue |> not then
                                    Profiling.addTimestamp
                                        $"+15d Engine.wrapAtomWithInterval. mount interval fn. atomValue={atomValue}. {getDebugInfo ()}"

                                    Atom.set setter cache atomValue
                                    lastValue <- Some atomValue

                        if intervalHandle = -1 then fn ()
                        intervalHandle <- JS.setInterval fn interval
                    })
                (fun _getter _setter ->
                    //                let logger = Logger.State.lastLogger
                    Profiling.addTimestamp $"+14d Engine.wrapAtomWithInterval unmount() {getDebugInfo ()}"

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
//                    Profiling.addTimestamp $"Engine.wrapAtomWithSubscription. onMount(). {getDebugInfo ()}"
//                    do! mount state
//                })
//            (fun _getter _setter _state ->
//                Profiling.addTimestamp $"Engine.wrapAtomWithSubscription onUnmount() {getDebugInfo ()}"
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

    let inline createRegisteredAtomWithGroupEx
        (storeAtomPath: StoreAtomPath)
        (defaultGroup: 'TGroup, defaultValue: 'A)
        : AtomConfig<(TicksGuid * ('TGroup * 'A)) list> =
        let getDebugInfo () =
            $"storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath} defaultValue={defaultValue}"

        Profiling.addTimestamp $"+13.1c Engine.createRegisteredAtomWithGroup [ constructor ] {getDebugInfo ()}"

        let defaultValue = Guid.newTicksGuid (), (defaultGroup, defaultValue)


        let groupMapAtom = Atom.atomFamilyAtom (fun (_alias: Gun.Alias option) -> defaultValue |> List.singleton)

        let rec lastSyncValueByTypeAtom =
            Atom.Primitives.readSelectorFamily
                (fun (alias: Gun.Alias option) getter ->
                    let groupMap = Atom.get getter (groupMapAtom alias)

                    groupValues groupMap)

        let wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    //                        let value = Atom.get getter subscriptions.[adapterType]

                    //                        syncEngine.SetProviders getter wrapper
                    let alias = Atom.get getter Selectors.Gun.alias

                    let userAtom = lastSyncValueByTypeAtom alias
                    let lastSyncValueByType = Atom.get getter userAtom

                    let result =
                        lastSyncValueByType
                        |> Seq.sortByDescending fst
                        |> Seq.toList

                    Profiling.addTimestamp
                        $"+13c Engine.createRegisteredAtomWithSort wrapper.get()  alias={alias} result={result} {getDebugInfo ()}"

                    result)
                (fun getter setter newValueFn ->

                    let alias = Atom.get getter Selectors.Gun.alias

                    Profiling.addTimestamp
                        $"+12c Engine.createRegisteredAtomWithSort wrapper.set() alias={alias} {getDebugInfo ()}  "

                    Atom.change setter (groupMapAtom alias) (unbox newValueFn))
            |> Atom.register storeAtomPath

        wrapper?init <- defaultValue

        wrapper

    let inline createRegisteredAtomWithGroup
        (storeAtomPath: StoreAtomPath)
        (defaultGroup: 'TGroup, defaultValue: 'A)
        : AtomConfig<TicksGuid * 'TGroup * 'A> =
        let getDebugInfo () =
            $"storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath} defaultValue={defaultValue}"

        Profiling.addTimestamp $"+13.1c Engine.createRegisteredAtomWithGroup [ constructor ] {getDebugInfo ()}"

        let defaultValue = Guid.newTicksGuid (), (defaultGroup, defaultValue)


        let groupMapAtom = Atom.atomFamilyAtom (fun (_alias: Gun.Alias option) -> defaultValue |> List.singleton)

        let rec lastSyncValueByTypeAtom =
            Atom.Primitives.readSelectorFamily
                (fun (alias: Gun.Alias option) getter ->
                    let groupMap = Atom.get getter (groupMapAtom alias)
                    groupValues groupMap)

        let wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    //                        let value = Atom.get getter subscriptions.[adapterType]

                    //                        syncEngine.SetProviders getter wrapper
                    let alias = Atom.get getter Selectors.Gun.alias

                    let userAtom = lastSyncValueByTypeAtom alias
                    let lastSyncValueByType = Atom.get getter userAtom

                    let ticks, (group: 'TGroup, result) =
                        lastSyncValueByType
                        |> Seq.sortByDescending fst
                        |> Seq.head

                    Profiling.addTimestamp
                        $"+13c Engine.createRegisteredAtomWithSort wrapper.get()  alias={alias} result={result} lastSyncValueByType={Json.encodeWithNull lastSyncValueByType} {getDebugInfo ()}"

                    ticks, group, result)
                (fun getter setter (ticks, group: 'TGroup, newValue) ->
                    let alias = Atom.get getter Selectors.Gun.alias

                    Atom.change
                        setter
                        (groupMapAtom alias)
                        (fun oldGroupValueList ->
                            let newGroupValueList = (ticks, (group, newValue)) :: oldGroupValueList

                            Profiling.addTimestamp
                                $"+12c Engine.createRegisteredAtomWithSort wrapper.set() ticks={ticks} alias={alias} newGroupValueList={newGroupValueList} oldGroupValueList={oldGroupValueList} {getDebugInfo ()} newValue={newValue} "

                            newGroupValueList))
            |> Atom.register storeAtomPath

        wrapper?init <- defaultValue

        wrapper

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

    let inline getAtomAdapter<'A> storeAtomPath adapterType =
        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

        let getDebugInfo adapterOptions =
            $"atomPath={atomPath} adapterOptions={adapterOptions} adapterType={adapterType}"

        match adapterType with
        | Atom.AdapterType.Gun ->
            (fun getter _setter adapterOptions setValue ->
                match adapterOptions with
                | Atom.AdapterOptions.Gun (_peers, alias) ->
                    let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (Some alias, atomPath))
                    let gunKeys = Atom.get getter Selectors.Gun.privateKeys

                    match gunAtomNode, gunKeys with
                    | Some gunAtomNode, Some gunKeys ->
                        Gun.batchSubscribe
                            gunAtomNode
                            (Guid.newTicksGuid ())
                            (fun (ticks, gunValue) ->
                                promise {
                                    let! newValue =
                                        match gunValue with
                                        | Gun.GunValue.EncryptedSignedValue result -> Gun.userDecode<'A> gunKeys result
                                        | _ -> unbox null |> Promise.lift

                                    //                                    setValue newValue
                                    try
                                        setValue (newValue |> Option.defaultValue (unbox null))
                                    with
                                    | ex ->
                                        Logger.logError
                                            (fun () -> $"gun subscrivbe data error ex={ex.Message} {getDebugInfo ()}")

                                        Logger.consoleError [| ex |]


                                    //                                        trigger (ticks, Atom.AdapterType.Gun, newValue)

                                    return! newHashedDisposable ticks
                                })
                    | _ -> failwith $"invalid gun atom node {getDebugInfo ()}"

                    Profiling.addTimestamp $"+11B ====> getAtomAdapterOptions gun mount {getDebugInfo ()} "
                | _ -> ()),
            (fun getter _setter adapterOptions ->
                match adapterOptions with
                | Atom.AdapterOptions.Gun (_peers, alias) ->
                    let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (Some alias, atomPath))

                    match gunAtomNode with
                    | Some gunAtomNode -> gunAtomNode.off () |> ignore
                    | _ -> ()

                    Profiling.addTimestamp $"+10B <==== getAtomAdapterOptions gun unmount  {getDebugInfo ()}  "
                | _ -> ())
        | Atom.AdapterType.Hub ->
            (fun _getter _setter adapterOptions _setValue ->
                match adapterOptions with
                | Atom.AdapterOptions.Hub (_alias, _hubUrl) ->
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                    Profiling.addTimestamp $"+09B ====> getAtomAdapterOptions hub mount  {getDebugInfo ()}  "
                | _ -> ()),
            (fun _getter _setter adapterOptions ->
                match adapterOptions with
                | Atom.AdapterOptions.Hub (_alias, _hubUrl) ->
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                    Profiling.addTimestamp $"+08B <==== getAtomAdapterOptions hub unmount  {getDebugInfo ()}  "
                | _ -> ()

                )
        | Atom.AdapterType.Memory ->
            (fun _getter _setter adapterOptions _setValue ->
                match adapterOptions with
                | Atom.AdapterOptions.Memory ->
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                    Profiling.addTimestamp $"+07B  ====> getAtomAdapterOptions memory mount  {getDebugInfo ()}  "
                | _ -> ()),
            (fun _getter _setter adapterOptions ->
                match adapterOptions with
                | Atom.AdapterOptions.Memory ->
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                    Profiling.addTimestamp $"+06B <==== getAtomAdapterOptions memory unmount  {getDebugInfo ()}  "
                | _ -> ())

    let inline getCollectionAdapter storeAtomPath adapterType =
        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

        let getDebugInfo adapterOptions =
            $"atomPath={atomPath} adapterOptions={adapterOptions} adapterType={adapterType}"

        match adapterType with
        | Atom.AdapterType.Gun ->
            (fun _getter _setter adapterOptions _setValue ->
                match adapterOptions with
                | Atom.AdapterOptions.Gun (_alias, _peers) ->
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                    Profiling.addTimestamp $"+11b ----> getCollectionAdapterOptions gun mount {getDebugInfo ()} "
                | _ -> ()),
            (fun _getter _setter adapterOptions ->
                match adapterOptions with
                | Atom.AdapterOptions.Gun (_alias, _peers) ->
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                    Profiling.addTimestamp $"+10b <---- getCollectionAdapterOptions gun unmount  {getDebugInfo ()}  "
                | _ -> ())
        | Atom.AdapterType.Hub ->
            (fun _getter _setter adapterOptions _setValue ->
                match adapterOptions with
                | Atom.AdapterOptions.Hub (_alias, _hubUrl) ->
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                    Profiling.addTimestamp $"+09b ----> getCollectionAdapterOptions hub mount  {getDebugInfo ()}  "
                | _ -> ()),
            (fun _getter _setter adapterOptions ->
                match adapterOptions with
                | Atom.AdapterOptions.Hub (_alias, _hubUrl) ->
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                    Profiling.addTimestamp $"+08b <---- getCollectionAdapterOptions hub unmount  {getDebugInfo ()}  "
                | _ -> ())
        | Atom.AdapterType.Memory ->
            (fun _getter _setter adapterOptions _setValue ->
                match adapterOptions with
                | Atom.AdapterOptions.Memory ->
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                    Profiling.addTimestamp $"+07b  ----> getCollectionAdapterOptions memory mount  {getDebugInfo ()}  "
                | _ -> ()),
            (fun _getter _setter adapterOptions ->
                match adapterOptions with
                | Atom.AdapterOptions.Memory ->
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                    Profiling.addTimestamp $"+06b <---- getCollectionAdapterOptions memory unmount  {getDebugInfo ()}  "
                | _ -> ())

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

    let inline createAdapterAtom storeAtomPath adapterType getAdapter (defaultValue: 'b) : AtomConfig<TicksGuid * 'b> =
        let mutable mounted = false

        let getDebugInfo () =
            $" storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath}"

        Atom.createRegistered storeAtomPath (AtomType.Atom (System.Guid.Empty, defaultValue))
        |> wrapAtomWithState
            (fun getter ->
                let adapterOptions = getAdapterOptions getter adapterType

                Profiling.addTimestamp
                    $"+04a * createAdapterAtom [ stateFn ] adapterType={adapterType} adapterOptions={Json.encodeWithNull adapterOptions} {getDebugInfo ()}"

                match adapterOptions with
                | Some adapterOptions ->
                    let mount, unmount = getAdapter storeAtomPath adapterType
                    // mount adapterOptions
                    Some (adapterType, adapterOptions, mount, unmount)
                | None -> None)
            (fun getter setter (adapterType, adapterOptions, mount, _) setAtom ->
                promise {
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                    Profiling.addTimestamp
                        $"+03a @@> createAdapterAtom mount adapterType={adapterType} adapterOptions={adapterOptions} {getDebugInfo ()}"

                    if not mounted then
                        mount getter setter adapterOptions setAtom
                        mounted <- true
                })
            (fun getter setter (adapterType, adapterOptions, _, unmount) ->
                Profiling.addTimestamp
                    $"+02a <@@ createAdapterAtom unmount adapterType={adapterType} adapterOptions={adapterOptions} {getDebugInfo ()} "

                if mounted then
                    unmount getter setter adapterOptions
                    mounted <- false)

    let collectionSubscriptionFamily =
        Atom.Primitives.atomFamily
            (fun (storeAtomPath, adapterType) ->
                createAdapterAtom storeAtomPath adapterType getCollectionAdapter ([||]: obj []))

    let inline subscribeCollection<'T when 'T: comparison>
        (storeRoot: StoreRoot)
        collection
        (_onFormat: TicksGuid -> 'T)
        : AtomConfig<AtomConfig<'T> []> =
        let storeAtomPath = CollectionAtomPath (storeRoot, collection)

        let getDebugInfo () =
            $"storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath}"

        Profiling.addTimestamp $"+05a subscribeCollection [ constructor ] {getDebugInfo ()}"

        let atom = createRegisteredAtomWithGroup storeAtomPath (Atom.AdapterType.Memory, [||])

        Atom.Primitives.readSelector
            (fun getter ->
                let adapterOptionsList =
                    Reflection.unionCases<Atom.AdapterType>
                    |> List.choose
                        (fun adapterType ->
                            match getAdapterOptions getter adapterType with
                            | Some adapterOptions -> Some (adapterType, adapterOptions)
                            | None -> None)

                let _adapterValues =
                    adapterOptionsList
                    |> List.toArray
                    |> Array.map (fun (adapterType, _) -> collectionSubscriptionFamily (storeAtomPath, adapterType))
                    |> Array.map (Atom.get getter)


                let ticks, adapterType, value = Atom.get getter atom

                Profiling.addTimestamp
                    $"+01a Engine.subscribeCollection wrapper.get() ticks={ticks} adapterType={adapterType} result={Json.encodeWithNull value} {getDebugInfo ()}"

                value)
        |> Atom.split

    let atomSubscriptionFamily =
        Atom.Primitives.atomFamily
            (fun (storeAtomPath, adapterType, defaultValue: obj) ->
                createAdapterAtom storeAtomPath adapterType getAtomAdapter defaultValue)

    let inline createRegisteredAtomWithSubscription storeAtomPath (defaultValue: 'A) : AtomConfig<'A> =
        //        atomAdapterSet.Add storeAtomPath |> ignore
        let localAdaptersAtom = createRegisteredAtomWithGroupEx storeAtomPath (Atom.AdapterType.Memory, defaultValue)

        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

        let getDebugInfo () =
            $"localAdaptersAtom={localAdaptersAtom} atomPath={atomPath}"

        Profiling.addTimestamp
            $"{nameof FsStore} | Engine.createRegisteredAtomWithSubscription [ constructor ] {getDebugInfo ()}"

        let mutable lastAdapterValues: (Atom.AdapterType * AtomConfig<TicksGuid * 'A> * (TicksGuid * 'A)) list = []
        let mutable lastLocalAdapter: (TicksGuid * (Atom.AdapterType * 'A)) option = None

        let sync () =
            match lastStore, lastLocalAdapter with
            | Some (getter, setter), Some (lastTicks: TicksGuid, (lastAdapterType: Atom.AdapterType, lastValue)) ->
                let values =
                    lastAdapterValues
                    |> List.map
                        (fun (adapterType, adapterAtom, (ticks2, adapterValue)) ->
                            adapterType,
                            (fun newValue -> Atom.set setter adapterAtom (Guid.newTicksGuid (), newValue)),
                            ticks2,
                            adapterValue)
                    |> List.append [
                        lastAdapterType,
                        (fun newValue ->
                            Atom.change
                                setter
                                localAdaptersAtom
                                (fun oldValue ->
                                    (Guid.newTicksGuid (), (lastAdapterType, newValue |> unbox<'A>))
                                    :: oldValue)),
                        lastTicks,
                        lastValue
                       ]
                    |> List.sortByDescending (fun (_, _, ticks, _) -> ticks)

                let valuesfmt =
                    Json.encodeWithNull (
                        values
                        |> List.map (fun (adapterType, _, ticks, value) -> adapterType, ticks, value)
                    )

                Profiling.addTimestamp
                    $"{nameof FsStore} | Engine.createRegisteredAtomWithSubscription [ sync ] {getDebugInfo ()} values={valuesfmt} "

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
                            if lastTicks > ticks then
                                // set adapter value from local atom

                                let newValue = lastValue

                                match adapterType with
                                | Atom.AdapterType.Gun ->
                                    let alias = Atom.get getter Selectors.Gun.alias
                                    let privateKeys = Atom.get getter Selectors.Gun.privateKeys
                                    let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))

                                    match privateKeys, gunAtomNode with
                                    | Some privateKeys, Some gunAtomNode ->
                                        let! newValueJson =
                                            promise {
                                                if newValue |> Js.ofNonEmptyObj |> Option.isNone then
                                                    return null
                                                else
                                                    let! (Gun.EncryptedSignedValue encrypted) =
                                                        newValue |> Gun.userEncode<'A> privateKeys

                                                    return encrypted
                                            }

                                        let! putResult =
                                            Gun.put
                                                gunAtomNode
                                                (Gun.GunValue.EncryptedSignedValue (
                                                    Gun.EncryptedSignedValue newValueJson
                                                ))

                                        if putResult then setAtom newValue
                                    | _ -> ()
                                | _ -> setAtom newValue
                            else
                                // set local atom from adapter value
                                lastSetAtom value

                            ()
                        })
                |> List.toArray
                |> Promise.all
                |> Promise.ignore
                |> Promise.start
            | _ -> ()

        let debouncedSync = Js.debounce sync 0

        let refreshAdapterValues getter =
            let result =
                Reflection.unionCases<Atom.AdapterType>
                |> List.choose
                    (fun adapterType ->
                        match getAdapterOptions getter adapterType with
                        | Some adapterOptions -> Some (adapterType, adapterOptions)
                        | None -> None)
                |> List.map
                    (fun (adapterType, _) ->
                        adapterType,
                        atomSubscriptionFamily (storeAtomPath, adapterType, defaultValue)
                        |> unbox<AtomConfig<TicksGuid * 'A>>)
                |> List.map
                    (fun (adapterType, adapterOptionsAtom) ->
                        adapterType, adapterOptionsAtom, Atom.get getter adapterOptionsAtom)

            lastAdapterValues <- result

            debouncedSync ()

        let rec wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    refreshAdapterValues getter

                    let localAdapters = Atom.get getter localAdaptersAtom
                    //                    lastLocalAdapters <- Some localAdapters

                    let _ticks, (_adapterType, value) = localAdapters |> List.head

                    lastLocalAdapter <- Some (_ticks, (_adapterType, value))

                    //                let result = Atom.get getter atom
//                    let ticks, adapterType, value = Atom.get getter adaptersAtom

                    Profiling.addTimestamp
                        $"+01a Engine.createRegisteredAtomWithSubscription wrapper.get() {getDebugInfo ()} localAdapters={Json.encodeWithNull localAdapters} lastAdapterValues={Json.encodeWithNull lastAdapterValues} "

                    //                snd result
                    value)
                (fun getter setter newValue ->
                    refreshAdapterValues getter

                    Atom.change
                        setter
                        localAdaptersAtom
                        (fun localAdapters ->
                            Profiling.addTimestamp
                                $"+00a Engine.createRegisteredAtomWithSubscription wrapper.set() {getDebugInfo ()} localAdapters={Json.encodeWithNull localAdapters} lastAdapterValues={Json.encodeWithNull lastAdapterValues}  "

                            let newTicks = Guid.newTicksGuid ()
                            let newEntry = newTicks, (Atom.AdapterType.Memory, newValue)
                            let newValue = newEntry :: localAdapters
                            lastLocalAdapter <- Some newEntry
                            newValue)

                    //                    _adapterValues
//                    |> Map.iter
//                        (fun currentAdapterType currentValue ->
//                            if (unbox newValue)
//                               |> Object.compare (unbox currentValue)
//                               |> not then
//                                Atom.set setter atom (newTicks, currentAdapterType, newValue)
                    )


        //        wrapper?init <- defaultValue

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

                            Profiling.addTimestamp "Engine.bindAtom. get(). choosing value2"
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

                            Profiling.addTimestamp "Engine.bindAtom. get(). choosing value1"
                            value1
                    | _ -> failwith $"bindAtom. atoms without default value. atom1={atom1} atom2={atom2}")
                (fun _get setter newValue ->
                    if lastValue.IsNone
                       || lastValue |> Object.compare (Some newValue) |> not then
                        lastValue <- Some newValue
                        Atom.set setter atom1 newValue

                        Profiling.addTimestamp "Engine.bindAtom. set(). setting value1"
                    else
                        Profiling.addTimestamp "Engine.bindAtom. set(). setting value2 only"

                    Atom.set setter atom2 newValue)

        wrapper?init <- atom1.init

        wrapper |> Atom.register storeAtomPath


    let inline createRegisteredAtomWithSubscriptionStorage storeAtomPath (defaultValue: 'A) =
        let storageAtom = Atom.createRegisteredWithStorage storeAtomPath defaultValue
        let syncAtom = createRegisteredAtomWithSubscription storeAtomPath defaultValue
        bindAtom syncAtom storageAtom







//        |> Atom.addSubscription
//                true
//                (fun setAtom ->
//                    lastSetAtom <- setAtom
//                    newMount ())
//                (fun () -> newUnmount ())



//                Reflection.unionCases<Atom.AdapterType>
//                |> List.toArray
//                |> Array.iter
//                    (fun adapterType ->
//
//                        Atom.set setter subscriptions.[adapterType] (Guid.newTicksGuid (), newValue)
//
//                        //                        atomArray
////                        |> Array.map
////                            (fun atom ->
////                                //                         let storeAtomPath = Atom.query (AtomReference.Atom atom)
//////                         storeAtomPath
////                                atom)
//                        )



//        let subscriptions =
//            Reflection.unionCases<Atom.AdapterType>
//            |> List.map
//                (fun adapterType ->
//                    let adapterCollection, mount, unmount = getAdapterCollection adapterType
//
//                    let storeAtomPath =
//                        IndexedAtomPath (
//                            FsStore.storeRoot,
//                            adapterCollection,
//                            [
//                                storeRoot |> StoreRoot.Value
//                            ],
//                            collection |> Collection.Value |> AtomName
//                        )
//
//                    let atomPath = storeAtomPath |> StoreAtomPath.AtomPath
//                    let getDebugInfo () = $"atomPath={atomPath}"
//
//                    let atoms =
//                        Atom.createRegistered storeAtomPath (AtomType.Atom ([||]: 'T []))
//                        |> wrapAtomWithSubscription
//                            (fun getter ->
//                                let adapterOptionsMap = getAdapterOptionsMap getter
//                                let adapterOptions = adapterOptionsMap.[adapterType]
//
//                                Profiling.addTimestamp
//                                    $"* subscribeCollection stateFn adapterOptions={adapterOptions} {getDebugInfo ()}"
//
//                                adapterOptions)
//                            (fun state ->
//                                promise {
//                                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
//                                    Profiling.addTimestamp
//                                        $"@@> subscribeCollection mount state={state} {getDebugInfo ()}"
//                                })
//                            (fun () -> Profiling.addTimestamp $"<@@ subscribeCollection unmount {getDebugInfo ()} ")
//                        |> Atom.split
//
//                    adapterType, atoms)
//            |> Map.ofList
//
//        Atom.Primitives.selector
//            (fun getter ->
//                Reflection.unionCases<Atom.AdapterType>
//                |> List.toArray
//                |> Array.collect
//                    (fun adapterType ->
//                        let atomArray = Atom.get getter subscriptions.[adapterType]
//
//                        atomArray
//                        |> Array.map
//                            (fun atom ->
//                                //                         let storeAtomPath = Atom.query (AtomReference.Atom atom)
////                         storeAtomPath
//                                atom)))
//            (fun getter setter newValue -> ())





//        let subscriptions =
//            Reflection.unionCases<Atom.AdapterType>
//            |> List.map
//                (fun adapterType ->
//                    let mount', unmount' = getAdapterOptions adapterType
//
//
//                    let atoms =
//                        Atom.createRegistered storeAtomPath (AtomType.Atom defaultValue)
//                        |> wrapAtomWithSubscription
//                            (fun getter ->
//                                let adapterOptionsMap = getAdapterOptionsMap getter
//                                let adapterOptions = adapterOptionsMap.[adapterType]
//
//                                Profiling.addTimestamp
//                                    $"* subscribeCollection stateFn adapterOptions={adapterOptions} {getDebugInfo ()}"
//
//                                adapterOptions)
//                            (fun state ->
//                                promise {
//                                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
//                                    Profiling.addTimestamp
//                                        $"@@> subscribeCollection mount state={state} {getDebugInfo ()}"
//
//                                    mount state
//                                    mount' state
//                                })
//                            (fun () ->
//                                Profiling.addTimestamp $"<@@ subscribeCollection unmount {getDebugInfo ()} "
//                                unmount' ()
//                                unmount ())
//
//                    adapterType, atoms)
//            |> Map.ofList

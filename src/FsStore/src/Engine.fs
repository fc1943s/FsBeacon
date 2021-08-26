namespace FsStore

open System.Collections.Generic
open Fable.Core
open FsCore.BaseModel
open FsJs
open FsStore.Bindings
open FsStore.Bindings.Jotai
open FsStore.Model
open FsStore.State
open FsCore

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

        Profiling.addTimestamp $">24e Engine.wrapAtomWithState [ constructor ] {getDebugInfo ()}"

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
                    Profiling.addTimestamp $">23e Engine.wrapAtomWithState newMount(). invoking mount {getDebugInfo ()}"
                    mounted <- true
                    do! mount getter setter state lastSetAtom
                | None ->
                    Profiling.addTimestamp
                        $">23e Engine.wrapAtomWithState newMount(). skipping, no state. {getDebugInfo ()}"
            }

        let newUnmount () =
            if mounted then
                match getState () with
                | Some (getter, setter, state) ->
                    Profiling.addTimestamp $">22e Engine.wrapAtom newUnmount(). invoking unmount {getDebugInfo ()}"
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
                        $">22e Engine.wrapAtom newUnmount(). skipping, no state. (should unmount here???) {getDebugInfo ()}"

        let refreshInternalState getter =
            if lastStore.IsNone then lastStore <- Atom.get getter Selectors.store

            let logger = Atom.get getter Selectors.logger
            Logger.State.lastLogger <- logger

            let newState = stateFn getter

            Profiling.addTimestamp
                $">21e Engine.wrapAtomWithState refreshInternalState (get or set). newState={Json.encodeWithNull newState}. will mount or unmount. {getDebugInfo ()}"

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

                    Profiling.addTimestamp $">20e Engine.wrapAtomWithState wrapper.get() {getDebugInfo ()}"

                    result)
                (fun getter setter newValue ->
                    refreshInternalState getter

                    Profiling.addTimestamp
                        $">19e Engine.wrapAtomWithState wrapper.set() newValue={newValue} {getDebugInfo ()}"

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

        atom
        |> wrapAtom
            (fun getter setter _setAtom ->
                promise {
                    let logger = Logger.State.lastLogger
                    Profiling.addTimestamp $">18d Engine.wrapAtomWithInterval. mount. {getDebugInfo ()}"

                    let fn () =
                        logger.Trace (fun () -> $"Engine.wrapAtomWithInterval. mount interval fn. {getDebugInfo ()}")

                        if intervalHandle >= 0 then
                            let atomValue = Atom.get getter atom

                            if Some atomValue |> Object.compare lastValue |> not then
                                Profiling.addTimestamp
                                    $">17d Engine.wrapAtomWithInterval. mount interval fn. atomValue={atomValue}. {getDebugInfo ()}"

                                Atom.set setter cache atomValue
                                lastValue <- Some atomValue

                    if intervalHandle = -1 then fn ()
                    intervalHandle <- JS.setInterval fn interval
                })
            (fun _getter _setter ->
                //                let logger = Logger.State.lastLogger
                Profiling.addTimestamp $">16d Engine.wrapAtomWithInterval unmount() {getDebugInfo ()}"

                if intervalHandle >= 0 then JS.clearTimeout intervalHandle
                intervalHandle <- -1)


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


    let inline groupAdapterValueMapByType adapterValueMap =
        let newMap =
            adapterValueMap
            |> Seq.groupBy (fun (_, adapterType, _) -> adapterType)
            |> Map.ofSeq
            |> Map.map
                (fun _ v ->
                    v
                    |> Seq.sortByDescending (fun (ticks, _, _) -> ticks)
                    |> Seq.map (fun (ticks, _, value) -> ticks, value)
                    |> Seq.head)

        Reflection.unionCases<Atom.AdapterType>
        |> List.map (fun case -> case, (newMap |> Map.tryFind case))
        |> Map.ofList


    let inline createRegisteredAtomWithAdapters<'T when 'T: comparison>
        (storeAtomPath: StoreAtomPath)
        (defaultValue: 'T)
        =
        let getDebugInfo () =
            $"storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath} defaultValue={defaultValue}"

        let adapterValueMapAtom =
            Atom.atomFamilyAtom
                (fun (_alias: Gun.Alias option) ->
                    [
                        Guid.newTicksGuid (), Atom.AdapterType.Memory, defaultValue
                    ])

        let rec lastSyncValueByTypeAtom =
            Atom.Primitives.readSelectorFamily
                (fun (alias: Gun.Alias option) getter ->
                    let adapterValueMap = Atom.get getter (adapterValueMapAtom alias)

                    groupAdapterValueMapByType adapterValueMap)

        Atom.Primitives.selector
            (fun getter ->
                //                        let value = Atom.get getter subscriptions.[adapterType]

                //                        syncEngine.SetProviders getter wrapper
                let alias = Atom.get getter Selectors.Gun.alias

                let userAtom = lastSyncValueByTypeAtom alias
                let lastSyncValueByType = Atom.get getter userAtom

                let adapterType, result =
                    lastSyncValueByType
                    |> Map.toSeq
                    |> Seq.choose
                        (fun (adapterType, values) ->
                            match values with
                            | Some (ticks, value) -> Some (ticks, (adapterType, value))
                            | None -> None)
                    |> Seq.sortByDescending fst
                    |> Seq.map snd
                    |> Seq.head

                Profiling.addTimestamp
                    $">14c Engine.createRegisteredAtomWithAdapters wrapper.get()  alias={alias} result={result} lastSyncValueByType={Json.encodeWithNull lastSyncValueByType} {getDebugInfo ()}"

                adapterType, result)
            (fun getter setter (adapterType, newValue) ->
                let alias = Atom.get getter Selectors.Gun.alias

                Atom.change
                    setter
                    (adapterValueMapAtom alias)
                    (fun oldAdapterValueList ->
                        let newAdapterValueList =
                            ((Guid.newTicksGuid ()), adapterType, newValue)
                            :: oldAdapterValueList

                        Profiling.addTimestamp
                            $">13c Engine.createRegisteredAtomWithAdapters wrapper.set()  alias={alias} newValue={newValue} newAdapterValueList={newAdapterValueList} oldAdapterValueList={oldAdapterValueList} {getDebugInfo ()}"

                        newAdapterValueList))
        |> Atom.register storeAtomPath



    let memoryAdapterOptions = Some Atom.AdapterOptions.Memory

    let getAdapterOptionsMap getter =
        [
            Atom.AdapterType.Gun, Atom.get getter Selectors.Gun.adapterOptions
            Atom.AdapterType.Hub, Atom.get getter Selectors.Hub.adapterOptions
            Atom.AdapterType.Memory, memoryAdapterOptions
        ]
        |> Map.ofList

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

    let inline subscribeCollection<'T when 'T: comparison>
        (storeRoot: StoreRoot)
        collection
        (_onFormat: TicksGuid -> 'T)
        : AtomConfig<AtomConfig<'T> []> =
        let storeAtomPath = CollectionAtomPath (storeRoot, collection)

        let getDebugInfo () =
            $"storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath}"

        Profiling.addTimestamp $">06a subscribeCollection [ constructor ] {getDebugInfo ()}"


        let subscriptionHandleMap = Dictionary<Atom.AdapterType, bool> ()

        Reflection.unionCases<Atom.AdapterType>
        |> List.iter (fun adapterType -> subscriptionHandleMap.Add (adapterType, false))

        let encodeSubscriptions subscriptions =
            subscriptions
            |> List.map (fun (adapterType, adapterOptions, _, _) -> adapterType, adapterOptions)
            |> Json.encodeWithNull

        let getAdapterOptions adapterType =
            let getDebugInfo adapterOptions =
                $"{getDebugInfo ()} adapterOptions={adapterOptions} adapterType={adapterType}"

            match adapterType with
            | Atom.AdapterType.Gun ->
                (fun _getter _setter adapterOptions _setValue ->
                    match adapterOptions with
                    | Atom.AdapterOptions.Gun (_alias, _peers) ->
                        //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                        Profiling.addTimestamp $">12b ----> getAdapterOptions gun mount {getDebugInfo ()} "
                    | _ -> ()),
                (fun _getter _setter ->
                    Profiling.addTimestamp $">11b <---- getAdapterOptions gun unmount  {getDebugInfo ()}  ")
            | Atom.AdapterType.Hub ->
                (fun _getter _setter adapterOptions _setValue ->
                    match adapterOptions with
                    | Atom.AdapterOptions.Hub (_alias, _hubUrl) ->
                        //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                        Profiling.addTimestamp $">10b ----> getAdapterOptions hub mount  {getDebugInfo ()}  "
                    | _ -> ()),
                (fun _getter _setter ->
                    Profiling.addTimestamp $">09b <---- getAdapterOptions hub unmount  {getDebugInfo ()}  ")
            | Atom.AdapterType.Memory ->
                (fun _getter _setter adapterOptions _setValue ->
                    match adapterOptions with
                    | Atom.AdapterOptions.Memory ->
                        //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                        Profiling.addTimestamp $">08b  ----> getAdapterOptions memory mount  {getDebugInfo ()}  "
                    | _ -> ()),
                (fun _getter _setter ->
                    Profiling.addTimestamp $">07b <---- getAdapterOptions memory unmount  {getDebugInfo ()}  ")

        let gunAtom =
            Atom.create (AtomType.Atom ([||]: 'T []))
            |> wrapAtomWithState
                (fun getter ->
                    let adapterType = Atom.AdapterType.Gun
                    let adapterOptionsMap = getAdapterOptionsMap getter
                    let adapterOptions = adapterOptionsMap.[adapterType]

                    Profiling.addTimestamp
                        $">05a *G subscribeCollection [ stateFn ] adapterOptions={Json.encodeWithNull adapterOptions} {getDebugInfo ()}"

                    match adapterOptions with
                    | Some adapterOptions ->
                        let mount, unmount = getAdapterOptions adapterType
                        // mount adapterOptions
                        Some (adapterType, adapterOptions, mount, unmount)
                    | None -> None)
                (fun getter setter (adapterType, adapterOptions, mount, _) setAtom ->
                    promise {
                        //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                        Profiling.addTimestamp
                            $">04a G@@> subscribeCollection mount adapterType={adapterType} adapterOptions={adapterOptions} {getDebugInfo ()}"

                        let setValue value = setAtom value

                        let mounted = subscriptionHandleMap.[adapterType]

                        if not mounted then
                            mount getter setter adapterOptions setValue
                            subscriptionHandleMap.[adapterType] <- true
                    })
                (fun getter setter (adapterType, adapterOptions, _, unmount) ->
                    Profiling.addTimestamp
                        $">03a <@@G subscribeCollection unmount adapterType={adapterType} adapterOptions={adapterOptions} {getDebugInfo ()} "

                    let mounted = subscriptionHandleMap.[adapterType]

                    if mounted then
                        unmount getter setter
                        subscriptionHandleMap.[adapterType] <- false)

        let memoryAtom =
            Atom.create (AtomType.Atom ([||]: 'T []))
            |> wrapAtomWithState
                (fun getter ->
                    let adapterType = Atom.AdapterType.Memory
                    let adapterOptionsMap = getAdapterOptionsMap getter
                    let adapterOptions = adapterOptionsMap.[adapterType]

                    Profiling.addTimestamp
                        $">05a *M subscribeCollection [ stateFn ] adapterOptions={Json.encodeWithNull adapterOptions} {getDebugInfo ()}"

                    match adapterOptions with
                    | Some adapterOptions ->
                        let mount, unmount = getAdapterOptions adapterType
                        // mount adapterOptions
                        Some (adapterType, adapterOptions, mount, unmount)
                    | None -> None)
                (fun getter setter (adapterType, adapterOptions, mount, _) setAtom ->
                    promise {
                        //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                        Profiling.addTimestamp
                            $">04a M@@> subscribeCollection mount adapterType={adapterType} adapterOptions={adapterOptions} {getDebugInfo ()}"

                        let setValue value = setAtom value

                        let mounted = subscriptionHandleMap.[adapterType]

                        if not mounted then
                            mount getter setter adapterOptions setValue
                            subscriptionHandleMap.[adapterType] <- true
                    })
                (fun getter setter (adapterType, adapterOptions, _, unmount) ->
                    Profiling.addTimestamp
                        $">03a <@@M subscribeCollection unmount adapterType={adapterType} adapterOptions={adapterOptions} {getDebugInfo ()} "

                    let mounted = subscriptionHandleMap.[adapterType]

                    if mounted then
                        unmount getter setter
                        subscriptionHandleMap.[adapterType] <- false)

        let atom =
            createRegisteredAtomWithAdapters storeAtomPath ([||]: 'T [])
            |> wrapAtomWithState
                (fun getter ->
                    let gunValue = Atom.get getter gunAtom
                    let memoryValue = Atom.get getter memoryAtom

                    Profiling.addTimestamp
                        $">05x * subscribeCollection [ stateFn ] gunValue={gunValue} memoryValue={memoryValue} {getDebugInfo ()}"

                    Some ())
                (fun getter setter subscriptions setAtom ->
                    promise {
                        //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                        Profiling.addTimestamp $">04a @@@@> subscribeCollection mount  {getDebugInfo ()}"
                    })
                (fun _getter _setter subscriptions ->
                    Profiling.addTimestamp $">03a <@@@@ subscribeCollection unmount  {getDebugInfo ()} ")

        Atom.Primitives.selector
            (fun getter ->
                let result = Atom.get getter atom

                Profiling.addTimestamp
                    $">02a Engine.subscribeCollection wrapper.get() result={Json.encodeWithNull result} {getDebugInfo ()}"

                snd result)
            (fun _getter setter newValue ->
                Profiling.addTimestamp
                    $">01a Engine.subscribeCollection wrapper.set() newValue={Json.encodeWithNull newValue} {getDebugInfo ()}"

                Atom.set setter atom (Atom.AdapterType.Memory, newValue))
        |> Atom.split
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

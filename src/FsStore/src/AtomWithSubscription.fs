namespace FsStore

open FsStore
open FsStore.Model
open FsStore.State
open Microsoft.FSharp.Core.Operators
open FsJs

#nowarn "40"


module AtomWithSubscription =
    let inline wrapAtomWithSubscription defaultValue mount unmount atom =
        let mutable lastAlias = None
        let mutable lastGunOptions = None
        let mutable lastGunAtomNode = None

        let storeAtomPath = null

        let getDebugInfo () =
            $"""
        | wrapAtomWithSubscription debugInfo:
        defaultValue={defaultValue}
        atom={atom}
        storeAtomPath={storeAtomPath}
        lastAlias={lastAlias}
        lastGunOptions={lastGunOptions}
        lastGunAtomNode={lastGunAtomNode} """

        let _getGunAtomNode () = ()
        //            Atom.get getter (Selectors.Gun.gunAtomNode (lastAlias, atomPath))

        atom
        |> Engine.wrapAtom
            (fun getter _setter _setAtom ->
                promise {
                    let logger = Logger.State.lastLogger
                    logger.Trace (fun () -> $"Store.wrapAtomWithSubscription. onMount() {getDebugInfo ()}")

                    lastAlias <- Atom.get getter Selectors.Gun.alias
                    lastGunOptions <- Some (Atom.get getter Atoms.gunOptions)
                    //                    lastGunAtomNode <- getGunAtomNode ()

                    logger.Trace (fun () -> $"Store.wrapAtomWithSubscription. #2 timeout  {getDebugInfo ()}")

                    do! mount ()
                })
            (fun _getter _setter ->
                let logger = Logger.State.lastLogger
                logger.Trace (fun () -> $"Store.wrapAtomWithSubscription onUnmount() {getDebugInfo ()}")
                unmount()

                ())


    let inline atomWithSubscription<'TValue  when 'TValue: equality>
        storeAtomPath
        (defaultValue: 'TValue)
        mount
        unmount
        atom
        : AtomConfig<'TValue> =
        let _atomPath = storeAtomPath |> StoreAtomPath.AtomPath

        atom
            |> wrapAtomWithSubscription defaultValue mount unmount
//
//        //        let atomStateMap = Store.atomFamily (fun (_: Gun.Alias option) -> AtomEngineState.Default)
//
//        //                let syncEngine = SyncEngine.Store.SyncEngine (defaultValue, Some (fun (key, node) -> key, node.back().back ()))
//
//        let mutable lastStore = None
//        let mutable lastAlias = None
//        let mutable lastGunOptions = None
//        let mutable lastGunAtomNode = None
//
//        let getDebugInfo () =
//            $"""
//        | atomWithSubscription debugInfo:
//        storeAtomPath={storeAtomPath}
//        defaultValue={defaultValue}
//        atom={atom}
//        lastAlias={lastAlias}
//        lastGunOptions={lastGunOptions}
//        lastGunAtomNode={lastGunAtomNode} """
//
//        let getGunAtomNode () =
//            match lastStore with
//            | Some (getter, _) -> Atom.get getter (Selectors.Gun.gunAtomNode (lastAlias, atomPath))
//            | None -> lastGunAtomNode
//
//        let refreshSubscriptions () =
//            promise {
//                Logger.logTrace (fun () -> $"Store.atomWithSubscription. refreshSubscriptions {getDebugInfo ()} ") }
//
//        let debouncedRefreshSubscriptions = Js.debounce (fun () -> refreshSubscriptions () |> Promise.start) 0
//
//        let refreshInternalState getter =
//            //            if lastAtomPath.IsNone then
////                lastAtomPath <- Some (Internal.queryAtomPath (AtomReference.Atom atom))
////
//            if lastStore.IsNone then lastStore <- Atom.get getter Selectors.store
//
//            Logger.State.lastLogger <- Atom.get getter Selectors.logger
//
//            lastAlias <- Atom.get getter Selectors.Gun.alias
//            lastGunOptions <- Some (Atom.get getter Atoms.gunOptions)
//            lastGunAtomNode <- getGunAtomNode ()
//
//            Logger.logTrace
//                (fun () -> $"Store.atomWithSubscription refreshInternalState. wrapper.get() {getDebugInfo ()} ")
//
//            match lastGunAtomNode with
//            | Some _ ->
//                printfn $"@@@@ subscription here {getDebugInfo ()}"
//                debouncedRefreshSubscriptions ()
//
//            //                match subscription with
////                | Some (_, None) ->
////                    Logger.logTrace
////                        (fun () ->
////                            $"Store.atomWithSubscription refreshInternalState. subscription and gun node present but no disposable. subscribing. {getDebugInfo ()}")
////                    debouncedSubscribe ()
////                | _ -> Logger.logTrace (fun () -> $"SyncEngine.SetProviders. gun node present. {getDebugInfo ()}")
//
//            | _ ->
//                Logger.logTrace
//                    (fun () -> $"Store.atomWithSubscription refreshInternalState. empty gun node {getDebugInfo ()} ")
//
//        let rec wrapper =
//            Atom.Primitives.selector
//                (fun getter ->
//                    refreshInternalState getter
//                    let result = Atom.get getter atom
//
//                    Logger.logTrace
//                        (fun () ->
//                            $"Store.atomWithSubscription wrapper.get() wrapper={wrapper} result={result} {getDebugInfo ()} ")
//
//                    result)
//                (fun getter setter newValueFn ->
//                    refreshInternalState getter
//
//                    Atom.set
//                        setter
//                        atom
//                        (unbox
//                            (fun oldValue ->
//                                let newValue = newValueFn |> Object.invokeOrReturnParam oldValue
//
//                                Logger.logTrace
//                                    (fun () ->
//                                        $"Store.atomWithSubscription wrapper.set() oldValue={oldValue} newValue={newValue}
//                                                newValueFn={newValueFn} wrapper={wrapper} {getDebugInfo ()} ")
//
//                                newValue)))
//            |> Atom.wrap
//                true
//                (fun _setAtom ->
//                    promise {
//                        Logger.logTrace
//                            (fun () ->
//                                $"Store.atomWithSubscription. internal subscribe wrapper={wrapper} {getDebugInfo ()} ")
//                    //            subscribe ()
//                    })
//                (fun () ->
//                    Logger.logTrace
//                        (fun () ->
//                            $"Store.atomWithSubscription. internal unsubscribe wrapper={wrapper} {getDebugInfo ()} "))
//
//
//        Logger.logTrace (fun () -> $"Store.atomWithSubscription constructor wrapper={wrapper} {getDebugInfo ()}")
//
//        wrapper?init <- defaultValue
//
//        wrapper

//    module SyncEngine =
//        module Store =
//            [<RequireQualifiedAccess>]
//            type AdapterSubscription =
//                | Gun of IGunChainReference
//                | Hub
//
//            [<RequireQualifiedAccess>]
//            type ValueKeyOperation =
//                | Add
//                | Remove
//
//            let adapterValueTimestampMap = Dictionary<AdapterType, int64> ()
//            let adapterValueMap = Dictionary<AdapterType, int64> ()
//
//            let valueKeyOperation = DateTime.Now.Ticks, AdapterType.Gun, ValueKeyOperation.Add
//
//            type SyncEngine<'T> (defaultValue: 'T, mapGunAtomNode) =
//                let internalAtom = Store.atomFamily (fun _alias -> defaultValue)
//                let mutable lastAtomPath = None
//                let mutable lastAccessors = None
//                let mutable lastAlias = None
//                let mutable lastGunOptions = None
//                let mutable lastGunAtomNode = None
//                let mutable lastHub = None
//                let mutable subscription = None
//
//                let mutable lastSubscribeParameters: (('T -> unit) -> SubscriptionId -> Fable.Core.JS.Promise<IDisposable option>) * ('T -> unit) =
//                    (fun _ _ -> failwith "no lastSubscribeParameters"), (fun _ -> failwith "lastSubscribeParameters")
//
//                let mutable n = 0
//
//                let getDebugSummary () =
//                    let result =
//                        $"{n}. lastAtomPath={lastAtomPath} lastGunOptions={lastGunOptions} subscription={subscription}"
//
//                    n <- n + 1
//                    result
//
//                let subscribe () =
//                    promise {
//                        let subscriptionId = SubscriptionId.NewId ()
//                        let subscribe, callback = lastSubscribeParameters
//
//                        Logger.logTrace
//                            (fun () ->
//                                $"SyncEngine.subscribe. before subscribe promise. lastAtomPath={lastAtomPath} subscription={subscription} lastGunAtomNode={lastGunAtomNode}")
//
//                        let! disposable = subscribe callback subscriptionId
//
//                        Logger.logTrace
//                            (fun () ->
//                                $"SyncEngine.subscribe. after subscribe promise. lastAtomPath={lastAtomPath} subscription={subscription} lastGunAtomNode={lastGunAtomNode}")
//                        //                            |> Promise.bind
//                        //                                (fun disposablePromise ->
//                        //                                    disposablePromise
//                        //                                    |> Option.defaultWith
//                        //                                        (fun () -> Store.newHashedDisposable (subscriptionId |> SubscriptionId.Value)))
//
//                        subscription <- Some (subscriptionId, disposable)
//
//                        if disposable.IsSome then
//                            Profiling.addCount (fun () -> $"@ {lastAtomPath}"
//
//                            match lastAccessors with
//                            | Some (getter, setter) ->
//                                //                            let! events =
//                                //                                StoreEngine.consumeMessages
//                                //                                    (Dom.Global.get "update" (fun _ _ _ _ ->
//                                //                                        eprintfn "warning: update is still not found"
//                                //                                        promise { () }
//                                //                                        ))
//                                //                                    getter
//                                //                                    setter
//                                //                                    null
//                                //                                    (Message.Command Command.Subscribe
//                                //                                     |> List.singleton)
//                                let events = null
//
//                                Logger.logTrace
//                                    (fun () ->
//                                        $"SyncEngine.subscribe. consumed. events={events} lastAtomPath={lastAtomPath} subscription={subscription} lastGunAtomNode={lastGunAtomNode}")
//                                //                            gunAtomNodeFromAtomPath getter lastAlias lastAtomPath
//                                //                            |> Option.map (mapGunAtomNode |> Option.defaultValue id)
//                                ()
//                            | None -> ()
//                            //                            lastGunAtomNode
//
//                            if (lastAtomPath
//                                |> Option.defaultValue (AtomPath "")
//                                |> AtomPath.Value)
//                                .Contains "/pub" then
//                                Profiling.addCount (fun () -> $"@@> {getDebugSummary ()}"
//                        else
//                            Logger.logTrace
//                                (fun () ->
//                                    $"SyncEngine.subscribe. no disposable returned from subscribe. lastAtomPath={lastAtomPath} subscription={subscription} lastGunAtomNode={lastGunAtomNode}")
//                    //                        Logger.logTrace (fun () -> $"SyncEngine.debouncedSubscribe. this={Json.encodeWithNull this} ")
//                    }
//
//                let debouncedSubscribe = Js.debounce (fun () -> subscribe () |> Promise.start) 0
//
//                member this.GetDebugSummary = getDebugSummary
//                member this.GetUserAtom<'T> () = internalAtom lastAlias
//                member this.GetAtomPath () = lastAtomPath
//                member this.GetAccessors () = lastAccessors
//                member this.GetAlias () = lastAlias
//                member this.GetGunOptions () = lastGunOptions
//
//                member this.GetGunAtomNode () =
//                    match lastAccessors with
//                    | Some (getter, _) ->
//                        Atom.value getter (Selectors.Gun.gunAtomNode (lastAlias, lastAtomPath))
//                        |> Option.map (mapGunAtomNode |> Option.defaultValue id)
//                    | None -> lastGunAtomNode
//
//                member this.GetHub () = lastHub
//                member this.GetSubscription () = subscription
//
//
//                member this.Subscribe (subscribe, callback) =
//                    lastSubscribeParameters <- subscribe, callback
//                    //
//                    //                Logger.logTrace
//                    //                    (fun () -> $"SyncEngine.Subscribe. debouncing from onMount... this={Json.encodeWithNull this} ")
//                    //
//                    debouncedSubscribe ()
//
//                member this.Unsubscribe fn =
//
//                    //                | Some (ticksGuid, None) ->
//                    //                    Logger.logTrace
//                    //                        (fun () ->
//                    //                            $"SyncEngine.Unsubscribe. onUnmount. skipping. has tick but no disposable. ticksGuid={ticksGuid} this={Json.encodeWithNull this}")
//
//                    match subscription with
//                    | Some (ticksGuid, disposable) ->
//                        //                    Logger.logTrace
//                        //                        (fun () ->
//                        //                            $"SyncEngine.Unsubscribe. onUnmount. unsubbing. ticksGuid={ticksGuid} this={Json.encodeWithNull this}")
//
//                        subscription <- None
//
//                        match disposable with
//                        | Some disposable ->
//                            (disposable: IDisposable).Dispose ()
//
//                            Profiling.removeCount $"@ {this.GetAtomPath ()}"
//
//                            if (lastAtomPath
//                                |> Option.defaultValue (AtomPath "")
//                                |> AtomPath.Value)
//                                .Contains "/pub" then
//                                Profiling.addCount (fun () -> $"@@< {getDebugSummary ()}"
//
//                            fn ticksGuid
//                        | None ->
//                            Logger.logTrace
//                                (fun () ->
//                                    $"SyncEngine.Unsubscribe. onUnmount. skipping. has tick but no disposable. ticksGuid={ticksGuid} this={Json.encodeWithNull this}")
//                    | _ -> ()
//                //                    Logger.logTrace
//                //                        (fun () -> $"SyncEngine.Unsubscribe. skipping unsub. this={Json.encodeWithNull this}")
//
//
//                member this.SetProviders getter atom =
//                    if lastAtomPath.IsNone then
//                        lastAtomPath <- Internal.queryAtomPath (AtomReference.Atom atom)
//
//                    if lastAccessors.IsNone then
//                        lastAccessors <- Atom.value getter Selectors.atomAccessors
//
//                    lastAlias <- Atom.value getter Selectors.Gun.alias
//                    lastGunOptions <- Some (Atom.value getter Atoms.gunOptions)
//                    Logger.State.lastLogger <- Atom.value getter Selectors.logger
//                    lastGunAtomNode <- this.GetGunAtomNode ()
//
//                    match lastAtomPath, lastGunAtomNode with
//                    | Some _, Some _ ->
//                        match subscription with
//                        | Some (_, None) ->
//                            Logger.logTrace
//                                (fun () ->
//                                    $"SyncEngine.SetProviders. subscription and gun node present but no disposable. subscribing. this={Json.encodeWithNull this}")
//
//                            debouncedSubscribe ()
//                        //                        debouncedSubscribe ()
//                        | _ ->
//                            Logger.logTrace
//                                (fun () ->
//                                    $"SyncEngine.SetProviders. gun node present.  this={Json.encodeWithNull this}")
//
//                        lastHub <- Atom.value getter Selectors.Hub.hub
//                    | _ -> ()
//
//    module SelectAtomSyncKeys =
//        module Store =
//            module Adapter =
//                module Gun =
//                    let adapter () = printfn $"Adapter.Gun.adapter"
//
//                module Hub =
//                    let adapter () = printfn $"Adapter.Hub.adapter"
//
//                module Jotai =
//                    let adapter () = printfn $"Adapter.Jotai.adapter"
//
//
//            //        let adapters =
//            //            [
//            //                Command.RegisterAdapter Adapter.Gun.adapter
//            //                Command.RegisterAdapter Adapter.Hub.adapter
//            //                Command.RegisterAdapter Adapter.Jotai.adapter
//            //            ]
//
//
//            let inline selectAtomSyncKeys
//                storeRoot
//                name
//                (atomFamily: 'TKey -> Atom<_>)
//                (key: 'TKey)
//                (onFormat: string -> 'TKey)
//                : Atom<Atom<'TKey> []> =
//
//                let atomKey =
//                    {
//                        StoreRoot = storeRoot
//                        Collection = None
//                        Keys = []
//                        Name = name
//                    }
//
//                let atomPath = atomKey |> AtomKey.AtomPath
//                let referenceAtom = atomFamily key
//
//                let syncEngine = SyncEngine.Store.SyncEngine ([||], Some (fun (key, node) -> key, node.back().back ()))
//
//
//                let getDebugInfo () =
//                    $"""
//        | selectAtomSyncKeys debugInfo:
//        syncEngine={Json.encodeWithNull syncEngine}
//        atomKey={Json.encodeWithNull atomKey}
//        referenceAtom={referenceAtom}
//        atomPath={atomPath} """
//
//                let mutable lastValue: Set<'TKey> option = None
//
//                let rec wrapper =
//                    Primitives.selector
//                        atomKey
//                        (fun getter ->
//                            syncEngine.SetProviders getter referenceAtom
//
//                            let result =
//                                if not Js.jestWorkerId then
//                                    Atom.value getter (syncEngine.GetUserAtom ())
//                                else
//                                    match syncEngine.GetAtomPath () with
//                                    | Some atomPath ->
//                                        match splitAtomPath atomPath with
//                                        | Some (root, _guid) ->
//                                            match testKeysCache.TryGetValue root with
//                                            | true, guids -> guids |> Set.toArray |> Array.map onFormat
//                                            | _ -> [||]
//                                        | None -> [||]
//                                    | None -> [||]
//
//                            Logger.logTrace
//                                (fun () ->
//                                    $"Store.selectAtomSyncKeys wrapper.get() wrapper={wrapper} result={result} {getDebugInfo ()} ")
//
//                            result)
//                        (fun getter setter newValueFn ->
//                            syncEngine.SetProviders getter referenceAtom
//
//                            Store.set
//                                setter
//                                (syncEngine.GetUserAtom ())
//                                (unbox
//                                    (fun oldValue ->
//                                        let newValue = newValueFn |> Object.invokeOrReturnParam oldValue
//
//                                        Logger.logTrace
//                                            (fun () ->
//                                                $"Store.selectAtomSyncKeys wrapper.set() oldValue={oldValue} newValue={newValue}
//                                                newValueFn={newValueFn} wrapper={wrapper} {getDebugInfo ()} ")
//
//                                        newValue)))
//
//                let atom1 = Jotai.jotai.atom 0
//
//                wrapper?onMount <- fun (setAtom: 'TKey [] -> unit) ->
//                                       match syncEngine.GetAccessors () with
//                                       | Some (getter, setter) ->
//                                           let value = Atom.value getter atom1
//
//                                           if (getDebugInfo ()).Contains "/pub" then
//                                               Profiling.addCount
//                                                   $">>> Mmount. setting value. atom={value} d:{syncEngine.GetDebugSummary ()}"
//
//                                               Atom.set setter atom1 (value + 1)
//
//                                       | None -> failwith "invalid accessors"
//
//                                       syncEngine.Subscribe (subscribe, setAtom)
//
//                                       fun _ ->
//                                           match syncEngine.GetAccessors () with
//                                           | Some (getter, setter) ->
//                                               let value = Atom.value getter atom1
//
//                                               if (getDebugInfo ()).Contains "/pub" then
//                                                   Profiling.addCount
//                                                       $"<<< Umount. setting value. atom={value} d:{syncEngine.GetDebugSummary ()}"
//
//                                                   Atom.set setter atom1 (value + 1)
//
//                                               syncEngine.Unsubscribe unsubscribe
//                                           | None -> failwith "invalid accessors"
//
//                Logger.logTrace
//                    (fun () ->
//                        $"Store.selectAtomSyncKeys constructor wrapper={wrapper} lastValue={lastValue} lastSubscription={lastSubscription} {getDebugInfo ()}")
//
//                wrapper?init <- [||]
//
//                splitAtom wrapper

namespace FsStore.Store

open FsStore
open Microsoft.FSharp.Core.Operators
open FsJs
open FsStore.Bindings.Jotai

#nowarn "40"


[<AutoOpen>]
module SelectAtomSyncKeys =
    module Store =
        module Adapter =
            module Gun =
                let adapter () = printfn $"Adapter.Gun.adapter"

            module Hub =
                let adapter () = printfn $"Adapter.Hub.adapter"

            module Jotai =
                let adapter () = printfn $"Adapter.Jotai.adapter"


        //        let adapters =
//            [
//                Command.RegisterAdapter Adapter.Gun.adapter
//                Command.RegisterAdapter Adapter.Hub.adapter
//                Command.RegisterAdapter Adapter.Jotai.adapter
//            ]


        let inline selectAtomSyncKeys2
            storeAtomPath
            //            name
//            (atomFamily: 'TKey -> Atom<_>)
//            (key: 'TKey)
            (_onFormat: string -> 'TKey)
            : AtomConfig<AtomConfig<'TKey> []> =



            let referenceAtom = Atom.Primitives.atom [||]

            AtomWithSubscription.atomWithSubscription
                storeAtomPath
                [||]
                (fun () ->
                    promise { Profiling.addTimestamp (fun () -> $"@ selectAtomSyncKeys subscribe {storeAtomPath}") })
                (fun () -> Profiling.addTimestamp (fun () -> $"@ selectAtomSyncKeys unsubscribe {storeAtomPath}"))
                referenceAtom
            |> Atom.split

//
//            let atomKey =
//                {
//                    StoreRoot = storeRoot
//                    Collection = None
//                    Keys = []
//                    Name = name
//                }
//
//            let atomPath = atomKey |> AtomKey.AtomPath
//            let referenceAtom = atomFamily key
//
//            let syncEngine = Store.SyncEngine ([||], Some (fun node -> node.back().back ()))
//
//
//            let getDebugInfo () =
//                $"""
//    | selectAtomSyncKeys debugInfo:
//    syncEngine={Json.encodeWithNull syncEngine}
//    atomKey={Json.encodeWithNull atomKey}
//    referenceAtom={referenceAtom}
//    atomPath={atomPath} """
//
//            let mutable lastValue: Set<'TKey> option = None
//
//            let rec wrapper =
//                Primitives.selector
//                    atomKey
//                    (fun getter ->
//                        syncEngine.SetProviders getter referenceAtom
//
//                        let result =
//                            if not Js.jestWorkerId then
//                                Atom.value getter (syncEngine.GetUserAtom ())
//                            else
//                                match syncEngine.GetAtomPath () with
//                                | Some atomPath ->
//                                    match splitAtomPath atomPath with
//                                    | Some (root, _guid) ->
//                                        match testKeysCache.TryGetValue root with
//                                        | true, guids -> guids |> Set.toArray |> Array.map onFormat
//                                        | _ -> [||]
//                                    | None -> [||]
//                                | None -> [||]
//
//                        Logger.logTrace
//                            (fun () ->
//                                $"Store.selectAtomSyncKeys wrapper.get() wrapper={wrapper} result={result} {getDebugInfo ()} ")
//
//                        result)
//                    (fun getter setter newValueFn ->
//                        syncEngine.SetProviders getter referenceAtom
//
//                        Store.set
//                            setter
//                            (syncEngine.GetUserAtom ())
//                            (unbox
//                                (fun oldValue ->
//                                    let newValue = newValueFn |> Object.invokeOrReturnParam oldValue
//
//                                    Logger.logTrace
//                                        (fun () ->
//                                            $"Store.selectAtomSyncKeys wrapper.set() oldValue={oldValue} newValue={newValue}
//                                            newValueFn={newValueFn} wrapper={wrapper} {getDebugInfo ()} ")
//
//                                    newValue)))
//
//            let mutable lastSubscription = None
//
//
//            let batchKeys setAtom data kind =
//                Gun.batchKeys
//                    (fun itemsArray ->
//                        let newSet = itemsArray |> Seq.collect snd |> Set.ofSeq
//
//                        let merge =
//                            match kind with
//                            | BatchKind.Replace -> newSet
//                            | BatchKind.Union ->
//                                lastValue
//                                |> Option.defaultValue Set.empty
//                                |> Set.union newSet
//
//                        lastValue <- Some merge
//                        let items = merge |> Set.toArray
//
//
//                        //                    let newItems =
//                        //                        itemsArray
//                        //                        |> Seq.collect snd
//                        //                        |> Seq.filter (lastSet.Contains >> not)
//                        //                        |> Seq.toArray
//                        //
//                        //                    let items =
//                        //                        itemsArray
//                        //                        |> Array.collect snd
//                        //                        |> Array.append newItems
//                        //                        |> Array.distinct
//                        //
//                        //                    lastValue <- Some (newItems |> Set.ofArray |> Set.union lastSet)
//
//                        Logger.logTrace
//                            (fun () ->
//                                $"Store.selectAtomSyncKeys. batchKeys callback. items.len={items.Length} atomPath={atomPath} key={key} {getDebugInfo ()} ")
//
//                        items)
//                    setAtom
//                    data
//
//            let subscribe setAtom subscriptionId : JS.Promise<IDisposable option> =
//                promise {
//                    match syncEngine.GetGunAtomNode (), lastSubscription with
//                    | _, Some _ ->
//                        Logger.logTrace
//                            (fun () ->
//                                $"Store.selectAtomSyncKeys subscribe. skipping subscribe, lastSubscription is set. key={key} {getDebugInfo ()} ")
//
//                        return None
//                    | Some gunAtomNode, None ->
//                        let batchKeysAtom (ticks, value) kind =
//                            batchKeys
//                                (fun value ->
//                                    setAtom value
//                                    newHashedDisposable ticks)
//                                (ticks, value)
//                                kind
//
//                        let gunSubscription =
//                            match syncEngine.GetGunOptions () with
//                            | Some (GunOptions.Sync _) ->
//                                gunAtomNode
//                                    .map()
//                                    .on (fun data (Gun.GunNodeSlice gunKey) ->
//                                        promise {
//                                            Logger.logTrace
//                                                (fun () ->
//                                                    $"Store.selectAtomSyncKeys subscribe. gun.map().on() result (inside disposable)
//                                          data={data} typeof data={jsTypeof data} gunKey={gunKey} typeof gunKey={jsTypeof gunKey}
//                                          atomPath={atomPath} syncEngine.atomPath={syncEngine.GetAtomPath ()} key={key} {getDebugInfo ()} ")
//
//                                            match data with
//                                            | Gun.GunValue.EncryptedSignedValue (Gun.EncryptedSignedValue (String.Valid _)) ->
//                                                let newValue =
//                                                    [|
//                                                        onFormat gunKey
//                                                    |]
//
//
//                                                batchKeysAtom (Guid.newTicksGuid (), newValue) BatchKind.Union
//                                            | _ -> eprintfn $"invalid gun.map().on() data={data}"
//                                        })
//
//                                //                        gunAtomNode.on
//                                //                            (fun data _key ->
//                                //                                let result =
//                                //                                    JS.Constructors.Object.entries data
//                                //                                    |> unbox<(string * obj) []>
//                                //                                    |> Array.filter
//                                //                                        (fun (guid, value) ->
//                                //                                            guid.Length = 36
//                                //                                            && guid <> string Guid.Empty
//                                //                                            && value <> null)
//                                //                                    |> Array.map fst
//                                //
//                                //                                if result.Length > 0 then
//                                //                                    setData result
//                                //                                else
//                                //                                    Dom.loggetLogger().Debug(fun () -> $"@@ atomKeys gun.on() API filter fetching/subscribing] @@@
//                                //                                    skipping. result.Length=0
//                                //                                    atomPath={atomPath} lastAtomPath={lastAtomPath} {key}")
//                                //                                    )
//
//                                lastSubscription <- Some DateTime.Now.Ticks
//                                Some ()
//                            | _ ->
//                                Logger.logTrace
//                                    (fun () ->
//                                        $"Store.selectAtomSyncKeys subscribe. gun.map().on() skipped. gun sync options disabled. (inside disposable) key={key} {getDebugInfo ()} ")
//
//                                None
//
//                        let hubSubscription =
//                            match syncEngine.GetAtomPath (), syncEngine.GetHub (), syncEngine.GetAlias () with
//                            | Some (AtomPath atomPath), Some hub, Some (Gun.Alias alias) ->
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
//                                        let collectionPath = alias, storeRoot, collection
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
//
//                        return
//                            match gunSubscription with
//                            | Some () ->
//                                Object.newDisposable
//                                    (fun () ->
//                                        promise {
//                                            Logger.logDebug
//                                                (fun () ->
//                                                    $"Store.selectAtomSyncKeys subscribe. returning disposable... subscriptionId={subscriptionId} atomPath={atomPath} key={key} {getDebugInfo ()} ")
//
//
//
//                                        //                        (db?data?find {| selector = {| key = atomPath |} |})?``$``?subscribe (fun items ->
//
//                                        //                        (collection?find ())?``$``?subscribe (fun items ->
//                                        //                            getLogger().Debug
//                                        //                                (fun () ->
//                                        //                                    $"@@ [wrapper.on() RX KEYS subscribe]
//                                        //                                    atomPath={atomPath}
//                                        //                                    items={JS.JSON.stringify items}
//                                        //                                            {baseInfo ()}
//                                        //                                         "))
//
//                                        }
//                                        |> Promise.start)
//                                |> Some
//                            | None -> None
//                    | None, _ ->
//                        Logger.logTrace
//                            (fun () ->
//                                $"Store.selectAtomSyncKeys subscribe. skipping, no gun atom node. {getDebugInfo ()}")
//
//                        return None
//
//
//
//                //                        (db?data?find {| selector = {| key = atomPath |} |})?``$``?subscribe (fun items ->
//
//                //                        (collection?find ())?``$``?subscribe (fun items ->
//                //                            getLogger().Debug
//                //                                (fun () ->
//                //                                    $"@@ [wrapper.on() RX KEYS subscribe]
//                //                                    atomPath={atomPath}
//                //                                    items={JS.JSON.stringify items}
//                //                                            {baseInfo ()}
//                //                                         "))
//
//                }
//
//            let unsubscribe _subscriptionId =
//                match syncEngine.GetGunAtomNode () with
//                | Some gunAtomNode ->
//
//                    Logger.logTrace
//                        (fun () ->
//                            $"Store.selectAtomSyncKeys. gunAtomNode found. calling off(). key={key} subscriptionId={_subscriptionId} {getDebugInfo ()} ")
//
//                    gunAtomNode.off () |> ignore
//                //                    lastSubscription <- None
//                | None ->
//                    Logger.logTrace
//                        (fun () ->
//                            $"Store.selectAtomSyncKeys [atomKeys gun.off()] skipping unsubscribe, no gun atom node. subscriptionId={_subscriptionId} {getDebugInfo ()} ")
//
//
//            let atom1 = jotai.atom 0
//
//            wrapper?onMount <- fun (setAtom: 'TKey [] -> unit) ->
//                                   let getter, setter = syncEngine.GetStore ()
//                                   let value = Atom.value getter atom1
//
//                                   if (getDebugInfo ()).Contains "/pub" then
//                                       Profiling.addCount
//                                           $">>> Mmount. setting value. atom={value} d:{syncEngine.GetDebugSummary ()}"
//
//                                       Atom.set setter atom1 (value + 1)
//
//                                   syncEngine.Subscribe (subscribe, setAtom)
//
//                                   fun _ ->
//                                       let getter, setter = syncEngine.GetStore ()
//                                       let value = Atom.value getter atom1
//
//                                       if (getDebugInfo ()).Contains "/pub" then
//                                           Profiling.addCount
//                                               $"<<< Umount. setting value. atom={value} d:{syncEngine.GetDebugSummary ()}"
//
//                                           Atom.set setter atom1 (value + 1)
//
//                                       syncEngine.Unsubscribe unsubscribe
//
//            Logger.logTrace
//                (fun () ->
//                    $"Store.selectAtomSyncKeys constructor wrapper={wrapper} lastValue={lastValue} lastSubscription={lastSubscription} {getDebugInfo ()}")
//
//            wrapper?init <- [||]
//
//            splitAtom wrapper

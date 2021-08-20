namespace FsStore.Store

open FsStore
open Fable.Core.JsInterop
open Fable.Core
open System
open FsStore.BaseStore.Store
open FsStore.Model
open FsBeacon.Shared
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs
open FsStore.Bindings
open FsStore.Bindings.Jotai

#nowarn "40"


[<AutoOpen>]
module SelectAtomSyncKeys =
    module Store =
        let inline selectAtomSyncKeys
            storeRoot
            name
            (atomFamily: 'TKey -> Atom<_>)
            (key: 'TKey)
            (onFormat: string -> 'TKey)
            : Atom<Atom<'TKey> []> =

            let atomKey =
                {
                    StoreRoot = storeRoot
                    Collection = None
                    Keys = []
                    Name = name
                }

            let atomPath = atomKey |> AtomKey.AtomPath
            let referenceAtom = atomFamily key

            let syncEngine = Store.SyncEngine (Some (fun (key, node) -> key, node.back().back ()))

            let internalAtom = jotaiUtils.atomFamily (fun _alias -> jotai.atom [||]) Object.compare

            let getDebugInfo () =
                $"""
    | selectAtomSyncKeys debugInfo:
    syncEngine={Json.encodeWithNull syncEngine}
    atomKey={Json.encodeWithNull atomKey}
    referenceAtom={referenceAtom}
    atomPath={atomPath}
    internalAtom[alias]={internalAtom (syncEngine.GetAlias ())} """

            let mutable lastValue: Set<'TKey> option = None

            let rec wrapper =
                Primitives.selector
                    atomKey
                    (fun getter ->
                        syncEngine.SetProviders getter referenceAtom
                        let userAtom = internalAtom (syncEngine.GetAlias ())

                        let result =
                            if not Js.jestWorkerId then
                                Store.value getter userAtom
                            else
                                match syncEngine.GetAtomPath () with
                                | Some atomPath ->
                                    match splitAtomPath atomPath with
                                    | Some (root, _guid) ->
                                        match testKeysCache.TryGetValue root with
                                        | true, guids -> guids |> Set.toArray |> Array.map onFormat
                                        | _ -> [||]
                                    | None -> [||]
                                | None -> [||]

                        Logger.logTrace
                            (fun () ->
                                $"Store.selectAtomSyncKeys wrapper.get() wrapper={wrapper} userAtom={userAtom} result={result} {getDebugInfo ()} ")

                        result)
                    (fun getter setter newValueFn ->
                        syncEngine.SetProviders getter referenceAtom
                        let userAtom = internalAtom (syncEngine.GetAlias ())

                        Store.set
                            setter
                            userAtom
                            (unbox
                                (fun oldValue ->
                                    let newValue = newValueFn |> Object.invokeOrReturnParam oldValue

                                    Logger.logTrace
                                        (fun () ->
                                            $"Store.selectAtomSyncKeys wrapper.set() newValue={newValue} newValueFn={newValueFn} wrapper={wrapper} userAtom={userAtom} {getDebugInfo ()} ")

                                    newValue)))

            let mutable lastSubscription = None


            let batchKeys setAtom data kind =
                Gun.batchKeys
                    (fun itemsArray ->
                        let newSet = itemsArray |> Seq.collect snd |> Set.ofSeq

                        let merge =
                            match kind with
                            | BatchKind.Replace -> newSet
                            | BatchKind.Union ->
                                lastValue
                                |> Option.defaultValue Set.empty
                                |> Set.union newSet

                        lastValue <- Some merge
                        let items = merge |> Set.toArray


                        //                    let newItems =
                        //                        itemsArray
                        //                        |> Seq.collect snd
                        //                        |> Seq.filter (lastSet.Contains >> not)
                        //                        |> Seq.toArray
                        //
                        //                    let items =
                        //                        itemsArray
                        //                        |> Array.collect snd
                        //                        |> Array.append newItems
                        //                        |> Array.distinct
                        //
                        //                    lastValue <- Some (newItems |> Set.ofArray |> Set.union lastSet)

                        Logger.logTrace
                            (fun () ->
                                $"Store.selectAtomSyncKeys. batchKeys callback. items.len={items.Length} atomPath={atomPath} key={key} {getDebugInfo ()} ")

                        items)
                    setAtom
                    data

            let subscribe (setAtom: 'TKey [] -> unit) =
                promise {
                    match syncEngine.GetGunAtomNode (), lastSubscription with
                    | _, Some _ ->
                        Logger.logTrace
                            (fun () ->
                                $"Store.selectAtomSyncKeys subscribe. skipping subscribe, lastSubscription is set. key={key} {getDebugInfo ()} ")
                    | Some (key, gunAtomNode), None ->
                        Logger.logTrace
                            (fun () ->
                                $"Store.selectAtomSyncKeys subscribe. subscribing. atomPath={atomPath} key={key} {getDebugInfo ()} ")

                        let batchKeysAtom (ticks, value) kind =
                            batchKeys
                                (fun value ->
                                    setAtom value
                                    newHashedDisposable ticks)
                                (ticks, value)
                                kind

                        match syncEngine.GetGunOptions () with
                        | Some (GunOptions.Sync _) ->
                            gunAtomNode
                                .map()
                                .on (fun data (Gun.GunNodeSlice gunKey) ->
                                    promise {
                                        Logger.logTrace
                                            (fun () ->
                                                $"Store.selectAtomSyncKeys gun.on() HUB filter fetching/subscribing] @@@ gunAtomNode.map().on result
                                          data={data} typeof data={jsTypeof data} gunKey={gunKey} typeof gunKey={jsTypeof gunKey}
                                          atomPath={atomPath} syncEngine.atomPath={syncEngine.GetAtomPath ()} key={key} {getDebugInfo ()} ")

                                        match data with
                                        | Gun.GunValue.EncryptedSignedValue (Gun.EncryptedSignedValue (String.Valid _)) ->
                                            let newValue =
                                                [|
                                                    onFormat gunKey
                                                |]


                                            batchKeysAtom (Guid.newTicksGuid (), newValue) BatchKind.Union
                                        | _ -> ()
                                    })

                            //                        gunAtomNode.on
                            //                            (fun data _key ->
                            //                                let result =
                            //                                    JS.Constructors.Object.entries data
                            //                                    |> unbox<(string * obj) []>
                            //                                    |> Array.filter
                            //                                        (fun (guid, value) ->
                            //                                            guid.Length = 36
                            //                                            && guid <> string Guid.Empty
                            //                                            && value <> null)
                            //                                    |> Array.map fst
                            //
                            //                                if result.Length > 0 then
                            //                                    setData result
                            //                                else
                            //                                    Dom.loggetLogger().Debug(fun () -> $"@@ atomKeys gun.on() API filter fetching/subscribing] @@@
                            //                                    skipping. result.Length=0
                            //                                    atomPath={atomPath} lastAtomPath={lastAtomPath} {key}")
                            //                                    )

                            lastSubscription <- Some DateTime.Now.Ticks
                        | _ ->
                            Logger.logTrace
                                (fun () ->
                                    $"Store.selectAtomSyncKeys gun.on() HUB filter fetching/subscribing] @@@ gunAtomNode.map().on skip.
                                      syncEngine.GetGunOptions() not in sync key={key} {getDebugInfo ()} ")

                        Logger.logTrace
                            (fun () ->
                                $"Store.selectAtomSyncKeys gun.on() HUB filter fetching/subscribing] @@@
                                atomPath={atomPath} syncEngine.atomPath={syncEngine.GetAtomPath ()} key={key} {getDebugInfo ()} ")

                        //                        (db?data?find {| selector = {| key = atomPath |} |})?``$``?subscribe (fun items ->
                        match syncEngine.GetAtomPath (), syncEngine.GetHub (), syncEngine.GetAlias () with
                        | Some (AtomPath atomPath), Some hub, Some (Gun.Alias alias) ->
                            promise {
                                try
                                    let storeRoot, collection =
                                        match atomPath |> String.split "/" |> Array.toList with
                                        | storeRoot :: [ _ ] -> Some storeRoot, None
                                        | storeRoot :: collection :: _ -> Some storeRoot, Some collection
                                        | _ -> None, None

                                    //                                hubSubscriptionMap
                                    match storeRoot, collection with
                                    | Some storeRoot, Some collection ->
                                        let collectionPath = alias, storeRoot, collection

                                        match Selectors.Hub.hubSubscriptionMap.TryGetValue collectionPath with
                                        | true, _sub ->
                                            Logger.logError
                                                (fun () -> $"Store.selectAtomSyncKeys sub already present key={key}")
                                        | _ ->
                                            let handle items =
                                                if items |> Array.isEmpty |> not then
                                                    Logger.logTrace
                                                        (fun () ->
                                                            $"Store.selectAtomSyncKeys gun.on() HUB filter fetching/subscribing] @@@
                                                                          setting keys locally. items.Length={items.Length}
                                                                          atomPath={atomPath} syncEngine.atomPath={syncEngine.GetAtomPath ()} key={key} {getDebugInfo ()} ")

                                                    batchKeysAtom
                                                        (Guid.newTicksGuid (), items |> Array.map onFormat)
                                                        BatchKind.Replace
                                                else
                                                    Logger.logTrace
                                                        (fun () ->
                                                            $"Store.selectAtomSyncKeys atomKeys gun.on() HUB filter fetching/subscribing] @@@
                                                                          skipping. items.Length=0
                                                                          atomPath={atomPath} syncEngine.atomPath={syncEngine.GetAtomPath ()} key={key} {getDebugInfo ()} ")


                                            Selectors.Hub.hubSubscriptionMap.[collectionPath] <- handle

                                            Gun.batchHubSubscribe
                                                hub
                                                (Sync.Request.Filter collectionPath)
                                                (Guid.newTicksGuid ())
                                                (fun (ticks, response: Sync.Response) ->
                                                    Logger.logTrace
                                                        (fun () ->
                                                            $"Store.selectAtomSyncKeys [wrapper.next() HUB keys stream subscribe] ticks={ticks} {getDebugInfo ()} response={response}")

                                                    promise {
                                                        match response with
                                                        | Sync.Response.FilterResult items ->
                                                            handle items

                                                            Logger.logTrace
                                                                (fun () ->
                                                                    $"Store.selectAtomSyncKeys [wrapper.on() HUB KEYS subscribe] atomPath={atomPath} items={JS.JSON.stringify items} {getDebugInfo ()} ")
                                                        | response ->
                                                            Logger.consoleError (
                                                                "Store.selectAtomSyncKeys Gun.batchHubSubscribe invalid response:",
                                                                response
                                                            )

                                                        return! newHashedDisposable ticks
                                                    })
                                                (fun _ex ->
                                                    Selectors.Hub.hubSubscriptionMap.Remove collectionPath
                                                    |> ignore)
                                    | _ ->
                                        Logger.consoleError
                                            $"Store.selectAtomSyncKeys #123561 invalid atom path atomPath={atomPath}"
                                with
                                | ex -> Logger.consoleError $"Store.selectAtomSyncKeys hub.filter, error={ex.Message}"
                            }
                            |> Promise.start

                        //                        (collection?find ())?``$``?subscribe (fun items ->
                        //                            getLogger().Debug
                        //                                (fun () ->
                        //                                    $"@@ [wrapper.on() RX KEYS subscribe]
                        //                                    atomPath={atomPath}
                        //                                    items={JS.JSON.stringify items}
                        //                                            {baseInfo ()}
                        //                                         "))
                        | _ ->
                            Logger.logTrace
                                (fun () ->
                                    $"Store.selectAtomSyncKeys [wrapper.on() RX KEYS subscribe]  skipping. {getDebugInfo ()}")

                    | None, _ ->
                        Logger.logTrace
                            (fun () ->
                                $"Store.selectAtomSyncKeys [atomKeys gun.on() subscribing] skipping subscribe, no gun atom node. {getDebugInfo ()}")
                }

            let debouncedSubscribe = Js.debounce (subscribe >> Promise.start) 100

            let unsubscribe () =
                match lastSubscription with
                | Some ticks when DateTime.ticksDiff ticks < 1000. ->
                    Logger.logTrace
                        (fun () ->
                            $"Store.selectAtomSyncKeys [atomKeys gun.off()] skipping unsubscribe. jotai resubscribe glitch. {getDebugInfo ()}")
                | Some _ ->
                    match syncEngine.GetGunAtomNode () with
                    | Some (key, _gunAtomNode) ->

                        Logger.logTrace
                            (fun () ->
                                $"Store.selectAtomSyncKeys [atomFamily.unsubscribe()] ############ (actually skipped) {key} {getDebugInfo ()} ")

                    //                    gunAtomNode.off () |> ignore
                    //                    lastSubscription <- None
                    | None ->
                        Logger.logTrace
                            (fun () ->
                                $"Store.selectAtomSyncKeys [atomKeys gun.off()] skipping unsubscribe, no gun atom node. {getDebugInfo ()} ")
                | None ->
                    Logger.logTrace
                        (fun () ->
                            $"Store.selectAtomSyncKeys [atomKeys gun.off()] skipping unsubscribe. no last subscription found. {getDebugInfo ()} ")

            let mutable lastSetAtom = None

            wrapper?onMount <- fun (setAtom: 'TKey [] -> unit) ->
                                   lastSetAtom <- Some setAtom

                                   Profiling.addCount $"@{syncEngine.GetAtomPath ()}"
                                   syncEngine.AddSubscriptionCount ()

                                   Logger.logTrace (fun () -> $"Store.selectAtomSyncKeys. onMount {getDebugInfo ()} ")

                                   //                               debouncedSubscribe setAtom

                                   fun _ ->
                                       Profiling.removeCount $"@{syncEngine.GetAtomPath ()}"
                                       syncEngine.RemoveSubscriptionCount ()

                                       Logger.logTrace
                                           (fun () -> $"Store.selectAtomSyncKeys. onUnmount {getDebugInfo ()} ")

            //                                   unsubscribe ()


            Logger.logTrace
                (fun () ->
                    $"Store.selectAtomSyncKeys constructor wrapper={wrapper} lastValue={lastValue} lastSubscription={lastSubscription} {getDebugInfo ()}")

            wrapper?init <- [||]

            splitAtom wrapper

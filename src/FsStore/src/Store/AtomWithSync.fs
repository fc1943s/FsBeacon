namespace FsStore.Store

open Fable.Core.JsInterop
open Fable.Core
open System
open FsStore
open FsStore.BaseStore.Store
open FsStore.Model
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs
open FsStore.Bindings
open FsStore.Bindings.Jotai

#nowarn "40"


[<AutoOpen>]
module AtomWithSync =
    module Store =
        let inline atomWithSync<'TKey, 'TValue> atomKey (defaultValue: 'TValue) =
            let mutable lastUserAtomId = None

            let syncEngine = Store.SyncEngine None
            let syncState = SyncState<'TValue> ()
            let atomPath = atomKey |> AtomKey.AtomPath

            let getDebugInfo () =
                $"""
    | atomWithSync debugInfo:
    syncState={Json.encodeWithNull syncState}
    syncEngine={Json.encodeWithNull syncEngine}
    atomKey={Json.encodeWithNull atomKey}
    lastUserAtomId={lastUserAtomId}
    defaultValue={defaultValue}
    atomPath={atomPath} """

            let adapterValueMapAtom =
                jotaiUtils.atomFamily
                    (fun (_alias: Gun.Alias option) ->
                        jotai.atom (
                            [
                                Guid.newTicksGuid (), AdapterValue.Internal defaultValue
                            ]
                            |> Map.ofList
                        ))
                    Object.compare

            let rec lastSyncValueByTypeAtom =
                Store.readSelectorFamily
                    FsStore.root
                    (nameof lastSyncValueByTypeAtom)
                    (fun (alias: Gun.Alias option) getter ->
                        let adapterValueMap = Store.value getter (adapterValueMapAtom alias)

                        groupAdapterValueMapByType adapterValueMap)

            //        let rec lastSyncValueAtom =
            //            Store.readSelectorFamily
            //                FsStore.root
            //                (nameof lastSyncValueAtom)
            //                (fun (alias: Alias option) getter ->
            //                    let lastSyncValueByTypeAtom = Store.value getter (lastSyncValueByTypeAtom alias)
            //
            //                    lastSyncValueByTypeAtom
            //                    |> Map.values
            //                    |> Seq.sortByDescending fst
            //                    |> Seq.head
            //                    |> snd)


            let syncTrigger (ticks, newValue) =
                match syncEngine.GetAccessors () with
                | Some (_, setter) ->
                    Store.change
                        setter
                        (adapterValueMapAtom (syncEngine.GetAlias ()))
                        (fun oldAdapterValueMap ->
                            Logger.logDebug
                                (fun () ->
                                    $"Store.atomWithSync.syncTrigger. setter. oldAdapterValueMap={oldAdapterValueMap} newValue={newValue}. {getDebugInfo ()}")

                            oldAdapterValueMap
                            |> (match newValue with
                                | Some value -> Map.add ticks value
                                | None -> Map.remove ticks))
                | None ->
                    Logger.logDebug
                        (fun () -> $"Store.atomWithSync.syncTrigger. skipped. no accessors. {getDebugInfo ()}")

            let debouncedSubscribe =
                Js.debounce
                    (fun () ->
                        let trigger (ticks, adapterValue: AdapterValue<'TValue> option) =
                            match syncState.AdapterValueMapByType with
                            | Some adapterValueMapByType ->
                                let trigger (ticks, newValue) =
                                    syncTrigger (ticks, newValue)
                                    newHashedDisposable ticks

                                let lastValue =
                                    match adapterValue with
                                    | Some (AdapterValue.Internal _) ->
                                        adapterValueMapByType
                                        |> Map.tryFind AdapterType.Internal
                                        |> Option.bind id
                                    | Some (AdapterValue.Gun _) ->
                                        adapterValueMapByType
                                        |> Map.tryFind AdapterType.Gun
                                        |> Option.bind id
                                    | Some (AdapterValue.Hub _) ->
                                        adapterValueMapByType
                                        |> Map.tryFind AdapterType.Hub
                                        |> Option.bind id
                                    | None -> None

                                let onError () =
                                    match adapterValue with
                                    | Some (AdapterValue.Gun _) -> syncState.GunSubscription <- None
                                    | Some (AdapterValue.Hub _) -> syncState.HubSubscription <- None
                                    | _ -> ()

                                Store.setInternalFromSync
                                    getDebugInfo
                                    trigger
                                    syncState.SyncPaused
                                    lastValue
                                    onError
                                    (ticks, adapterValue)
                            | None -> ()

                        let rec onError () =
                            syncState.HubSubscription <- None

                            JS.setTimeout
                                (fun () ->
                                    Store.syncSubscribe getDebugInfo syncEngine syncState trigger onError atomPath
                                    |> Promise.start)
                                1000
                            |> ignore

                        Store.syncSubscribe getDebugInfo syncEngine syncState trigger onError atomPath
                        |> Promise.start)
                    100

            let batchPutFromUi (ticks, newValue) =
                Batcher.batch (
                    Batcher.BatchType.Set (
                        ticks,
                        (fun ticks -> Store.putFromUi getDebugInfo syncEngine syncTrigger (ticks, newValue))
                    )
                )

            let debouncedPutFromUi = Js.debounce batchPutFromUi 100

            let rec wrapper =
                Primitives.selector
                    atomKey
                    (fun getter ->
                        syncEngine.SetProviders getter wrapper

                        //                    let userAtom = lastSyncValueAtom (syncEngine.GetAlias ())
                        let userAtom = lastSyncValueByTypeAtom (syncEngine.GetAlias ())
                        let lastSyncValueByType = Store.value getter userAtom

                        syncState.AdapterValueMapByType <- Some lastSyncValueByType

                        let result =
                            lastSyncValueByType
                            |> Map.values
                            |> Seq.choose id
                            |> Seq.sortByDescending fst
                            |> Seq.head
                            |> snd

                        Profiling.addCount $"{atomPath} get"

                        Logger.logTrace
                            (fun () ->
                                if (string result
                                    |> Option.ofObjUnbox
                                    |> Option.defaultValue "")
                                    .StartsWith "Ping " then
                                    null
                                else
                                    $"atomFamily.wrapper.get() wrapper={wrapper} userAtom={userAtom} result={result} {getDebugInfo ()}               ")

                        Logger.logTrace
                            (fun () ->
                                if (string result
                                    |> Option.ofObjUnbox
                                    |> Option.defaultValue "")
                                    .StartsWith "Ping " then
                                    null
                                else
                                    $"atomFamily.wrapper.get() wrapper={wrapper} userAtom={userAtom} result={result} {getDebugInfo ()}               ")

                        let userAtomId = Some (userAtom.toString ())

                        if userAtomId <> lastUserAtomId then
                            lastUserAtomId <- userAtomId

                            match syncState.GunSubscription with
                            | None ->
                                Logger.logTrace
                                    (fun () ->
                                        $"atomFamily.wrapper.get() subscribing wrapper={wrapper} userAtom={userAtom} {getDebugInfo ()}                       ")

                                debouncedSubscribe ()
                            | _ ->
                                Logger.logTrace
                                    (fun () ->
                                        $"atomFamily.wrapper.get() skipping subscribe wrapper={wrapper} userAtom={userAtom} {getDebugInfo ()}                           ")

                        result)
                    (fun getter setter newValueFn ->
                        syncEngine.SetProviders getter wrapper
                        Profiling.addCount $"{atomPath} set"

                        Store.change
                            setter
                            (adapterValueMapAtom (syncEngine.GetAlias ()))
                            (fun oldAdapterValueMap ->
                                let newValue = newValueFn |> Object.invokeOrReturn

                                let newAdapterValueMap =
                                    oldAdapterValueMap
                                    |> Map.add (Guid.newTicksGuid ()) (AdapterValue.Internal newValue)

                                let adapterValueMapByType = groupAdapterValueMapByType oldAdapterValueMap

                                syncState.AdapterValueMapByType <- Some adapterValueMapByType


                                //                                if true
                                //                                   || oldValue |> Object.compare newValue |> not
                                //                                   || (lastValue.IsNone
                                //                                       && newValue |> Object.compare defaultValue) then
                                let newValueOption = newValue |> Option.ofObjUnbox

                                let gunValue =
                                    adapterValueMapByType
                                    |> Map.tryFind AdapterType.Gun
                                    |> Option.bind id
                                    |> Option.map snd

                                let hubValue =
                                    adapterValueMapByType
                                    |> Map.tryFind AdapterType.Hub
                                    |> Option.bind id
                                    |> Option.map snd

                                Logger.logTrace
                                    (fun () ->
                                        $"<filter> atomFamily.wrapper.set()
    wrapper={wrapper} oldAdapterValueMap={oldAdapterValueMap} newValue={newValue} jsTypeof-newValue={jsTypeof newValue}
    adapterValueMapByType={adapterValueMapByType} __x={(newValueOption, gunValue, hubValue)}
    y={unbox newValueOption = unbox gunValue
       && unbox gunValue = unbox hubValue}
    z={box newValueOption = box gunValue
       && box gunValue = box hubValue}
    {getDebugInfo ()}                                           ")



                                if box newValueOption = box gunValue
                                   && box gunValue = box hubValue then
                                    Logger.logTrace
                                        (fun () ->
                                            $"<filter> atomFamily.wrapper.set(). skipped debouncedPut
    wrapper={wrapper} oldAdapterValueMap={oldAdapterValueMap} newValue={newValue} jsTypeof-newValue={jsTypeof newValue} {getDebugInfo ()} ")
                                else

                                    syncState.SyncPaused <- true
                                    debouncedPutFromUi (Guid.newTicksGuid (), newValue)

                                if Js.jestWorkerId then
                                    match splitAtomPath atomPath with
                                    | Some (root, guid) ->
                                        let newSet =
                                            match testKeysCache.TryGetValue root with
                                            | true, guids -> guids |> Set.add guid
                                            | _ -> Set.singleton guid

                                        testKeysCache.[root] <- newSet
                                    | None -> ()

                                newAdapterValueMap))

            Logger.logTrace
                (fun () ->
                    $"Store.atomWithSync constructor
    adapterValueMapAtom[alias]={(adapterValueMapAtom (syncEngine.GetAlias ()))}
    lastSyncValueByTypeAtom[alias]={(lastSyncValueByTypeAtom (syncEngine.GetAlias ()))} wrapper={wrapper} {getDebugInfo ()}")

            if atomKey.Keys
               <> (string Guid.Empty |> List.singleton) then
                wrapper?onMount <- fun (_setAtom: 'TValue option -> unit) ->
                                       debouncedSubscribe ()

                                       fun () ->
                                           Store.syncUnsubscribe
                                               getDebugInfo
                                               (syncEngine.GetGunAtomNode ())
                                               syncState.GunSubscription
                                               (fun () -> syncState.GunSubscription <- None)

            wrapper?init <- defaultValue

            Internal.registerAtom Internal.AtomType.AtomWithSync atomPath wrapper

            wrapper

        let inline atomFamilyWithSync<'TKey, 'TValue>
            storeRoot
            collection
            name
            (defaultValueFn: 'TKey -> 'TValue)
            keysIdentifier
            =
            jotaiUtils.atomFamily
                (fun param ->
                    atomWithSync<'TKey, 'TValue>
                        {
                            StoreRoot = storeRoot
                            Collection = Some collection
                            Keys = keysIdentifier param
                            Name = name
                        }
                        (defaultValueFn param))
                Object.compare

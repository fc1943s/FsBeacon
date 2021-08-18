namespace FsStore

open System.Collections.Generic
open Fable.Extras
open Fable.Core.JsInterop
open Fable.Core
open System
open FsCore.BaseModel
open FsStore.Model
open FsBeacon.Shared
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs
open FsStore.Bindings
open FsStore.Bindings.Jotai

#nowarn "40"


module Store =
    let rec readSelectorInterval storeRoot name interval defaultValue getFn =
        let cache = jotai.atom defaultValue

        let mutable lastAccessors = None
        let mutable timeout = -1

        let getDebugInfo () =
            $"
| readSelectorInterval baseInfo:
storeRoot/name={storeRoot}/{name}
interval={interval}
defaultValue={defaultValue}
lastAccessors={lastAccessors.IsSome}
timeout={timeout} "

        Logger.logTrace (fun () -> $"readSelectorInterval.constructor {getDebugInfo ()}")

        let readSelector = Store.readSelector storeRoot name getFn

        let rec readSelectorWrapper =
            Store.readSelector
                storeRoot
                $"{name}_{nameof readSelectorWrapper}"
                (fun getter ->
                    if lastAccessors.IsNone then
                        lastAccessors <- Store.value getter Selectors.atomAccessors

                    Logger.State.lastLogger <- Store.value getter Selectors.logger
                    let cache = Store.value getter cache

                    Logger.logTrace
                        (fun () ->
                            $"readSelectorInterval.wrapper.get() cache={cache |> Option.ofObjUnbox |> Option.isSome} {getDebugInfo ()}")

                    cache)

        let mutable lastValue = None

        let subscribe () =
            Logger.logTrace (fun () -> $"readSelectorInterval.onMount() {getDebugInfo ()}")

            let fn () =
                Logger.logTrace (fun () -> $"#1 readSelectorInterval.timeout {getDebugInfo ()}")

                match lastAccessors with
                | Some (getter, setter) when timeout >= 0 ->
                    let selectorValue = Store.value getter readSelector

                    if Some selectorValue
                       |> Object.compare lastValue
                       |> not then
                        Logger.logTrace
                            (fun () ->
                                $"#2 readSelectorInterval.timeout selectorValue={selectorValue
                                                                                 |> Option.ofObjUnbox
                                                                                 |> Option.isSome} {getDebugInfo ()}")

                        Store.set setter cache selectorValue
                        lastValue <- Some selectorValue
                | _ -> ()

            if timeout = -1 then fn ()
            timeout <- JS.setInterval fn interval

        let unsubscribe () =
            Logger.logTrace (fun () -> $"readSelectorInterval.onUnmount() {getDebugInfo ()}")

            if timeout >= 0 then JS.clearTimeout timeout
            timeout <- -1

        readSelectorWrapper?onMount <- fun _setAtom ->
                                           subscribe ()
                                           fun () -> unsubscribe ()

        readSelectorWrapper?init <- defaultValue

        readSelectorWrapper

    let inline readSelectorFamilyInterval storeRoot name interval defaultValue getFn =
        jotaiUtils.atomFamily
            (fun param -> readSelectorInterval storeRoot name interval defaultValue (getFn param))
            Object.compare

    let inline gunAtomNodeFromAtomPath getter alias atomPath =
        match alias, atomPath with
        | Some alias, Some atomPath ->
            match Store.value getter (Selectors.Gun.gunAtomNode (alias, atomPath)) with
            | Some gunAtomNode -> Some ($">> atomPath={atomPath} alias={alias}", gunAtomNode)
            | _ -> None
        | _ -> None


    type SyncEngine (mapGunAtomNode) =
        let mutable lastAtomPath = None
        let mutable lastAccessors = None
        let mutable lastAlias = None
        let mutable lastGunOptions = None
        let mutable lastGunAtomNode = None
        let mutable lastHub = None

        member this.GetAtomPath () = lastAtomPath
        member this.GetAccessors () = lastAccessors
        member this.GetAlias () = lastAlias
        member this.GetGunOptions () = lastGunOptions
        member this.GetGunAtomNode () = lastGunAtomNode
        member this.GetHub () = lastHub

        member this.SetProviders getter atom =
            if lastAtomPath.IsNone then
                lastAtomPath <- Internal.queryAtomPath (AtomReference.Atom atom)

            Profiling.addCount $"createSyncEngine.setProviders {lastAtomPath}"

            if lastAccessors.IsNone then
                lastAccessors <- Store.value getter Selectors.atomAccessors

            lastAlias <- Store.value getter Selectors.Gun.alias
            lastGunOptions <- Some (Store.value getter Atoms.gunOptions)
            Logger.State.lastLogger <- Store.value getter Selectors.logger

            lastGunAtomNode <-
                gunAtomNodeFromAtomPath getter lastAlias lastAtomPath
                |> Option.map (mapGunAtomNode |> Option.defaultValue id)

            match lastAtomPath, lastGunAtomNode with
            | Some _, Some _ -> lastHub <- Store.value getter Selectors.Hub.hub
            | _ -> ()

            match lastAtomPath with
            | Some (AtomPath atomPath) ->
                Logger.logTrace
                    (fun () ->
                        $"SyncEngine.SetProviders() atom={atom} atomPath={atomPath} this={Json.encodeWithNull this}")
            | _ -> ()




    let testKeysCache = Dictionary<string, Set<string>> ()

    let inline splitAtomPath (AtomPath atomPath) =
        let matches =
            (JSe.RegExp @"(.*?)\/([\w-]{36})\/\w+.*?")
                .Match atomPath
            |> Option.ofObj
            |> Option.defaultValue Seq.empty
            |> Seq.toList

        match matches with
        | _match :: root :: guid :: _key -> Some (root, guid)
        | _ -> None

    let inline newHashedDisposable (ticks: TicksGuid) =
        promise {
            Logger.logDebug (fun () -> $"emptyDisposableFromTrigger() ticks={ticks}")

            return
                Object.newDisposable
                    (fun () -> Logger.logDebug (fun () -> $"emptyDisposableFromTrigger Dispose. ticks={ticks}"))
        }

    let inline splitAtom atom = jotaiUtils.splitAtom atom


    let inline syncUnsubscribe getDebugInfo gunAtomNode subscription success =
        match subscription with
        | Some ticks when DateTime.ticksDiff ticks < 1000. ->
            Logger.logTrace
                (fun () ->
                    $"[syncUnsubscribe] skipping unsubscribe. jotai resubscribe glitch.
                        gunAtomNode={gunAtomNode} {getDebugInfo ()} ")
        | Some _ ->
            match gunAtomNode with
            | Some (key, gunAtomNode: Gun.Types.IGunChainReference) ->

                Profiling.addCount $"{key} unsubscribe"

                Logger.logTrace
                    (fun () ->
                        $"[syncUnsubscribe] {key} (######## actually skipped)
                            gunAtomNode={gunAtomNode} {getDebugInfo ()} ")

                gunAtomNode.off () |> ignore
                success ()
            | None ->
                Logger.logTrace
                    (fun () ->
                        $"[syncUnsubscribe] skipping unsubscribe, no gun atom node.
                            gunAtomNode={gunAtomNode} {getDebugInfo ()} ")

        | None ->
            Logger.logTrace
                (fun () ->
                    $"[syncUnsubscribe] skipping unsubscribe. no last subscription found.
                            gunAtomNode={gunAtomNode} {getDebugInfo ()} ")



    //    type Msg =
//        | Internal of int64 option * obj
//        | Gun of int64 * obj
//        | Hub of int64 * obj

    [<RequireQualifiedAccess>]
    type AdapterType =
        | Internal
        | Gun
        | Hub

    [<RequireQualifiedAccess>]
    type AdapterValue<'T> =
        | Internal of 'T
        | Gun of 'T
        | Hub of 'T

    type Event<'T> =
        | UserCreated
        | UserSignedIn
        | AdapterEnable
        | AdapterSubscribe
        | AdapterValue of AdapterValue<'T>
        | AdapterUnsubscribe
        | AdapterDisable

    type AdapterValueMap2<'T> = Map<TicksGuid, AdapterValue<'T>>
    type Z = Map<AdapterType, TicksGuid * Gun.EncryptedSignedValue> //selector, defaultValue
    type R = TicksGuid -> Gun.EncryptedSignedValue
    type F = Gun.EncryptedSignedValue

    //    type AckMap = Map<AdapterValue<'T>, > //selector, defaultValue

    type Y =
        | Y //of Msg
        | W

    type AtomSyncState<'T> = { Value: 'T }


    type SyncState<'TValue> () =
        let mutable lastAdapterValueMapByType: Map<AdapterType, (TicksGuid * 'TValue) option> option = None
        let mutable lastGunSubscription = None
        let mutable lastHubSubscription = None
        let mutable syncPaused = false

        member this.AdapterValueMapByType
            with get () = lastAdapterValueMapByType
            and set value = lastAdapterValueMapByType <- value

        member this.GunSubscription
            with get () = lastGunSubscription
            and set value = lastGunSubscription <- value

        member this.HubSubscription
            with get () = lastHubSubscription
            and set value = lastHubSubscription <- value

        member this.SyncPaused
            with get () = syncPaused
            and set value = syncPaused <- value


    let inline setInternalFromSync getDebugInfo setAtom syncPaused lastValue onError (ticks, newValue) =
        try
            Logger.logTrace
                (fun () ->
                    $"gun.on() value. start.
newValue={newValue} jsTypeof-newValue={jsTypeof newValue} lastValue={lastValue} ticks={ticks} {getDebugInfo ()}")

            match syncPaused, lastValue with
            | true, _ ->
                Logger.logTrace
                    (fun () ->
                        $"gun.on() value. skipping. Sync paused.
newValue={newValue} jsTypeof-newValue={jsTypeof newValue} lastValue={lastValue} ticks={ticks} {getDebugInfo ()}")
            | _, Some (lastValueTicks, lastValue) when
                match lastValue |> Option.ofObjUnbox, newValue |> Option.ofObjUnbox with
                | _, _ when lastValueTicks > ticks -> true
                | lastValue, newValue when lastValue |> Object.compare newValue -> true
                | Some _, None -> true
                | None, None -> true
                | _ -> false
                ->

                Logger.logTrace
                    (fun () ->
                        $"gun.on() value. skipping.
newValue={newValue} jsTypeof-newValue={jsTypeof newValue} lastValue={lastValue} ticks={ticks} {getDebugInfo ()} ")
            | _ ->
                if unbox newValue = JS.undefined then
                    Logger.logTrace
                        (fun () ->
                            $"gun.on() value. skipping. newValue=undefined
newValue={newValue} jsTypeof-newValue={jsTypeof newValue} lastValue={lastValue} ticks={ticks} {getDebugInfo ()}")
                else
                    try
                        Logger.logTrace
                            (fun () ->
                                let _lastValue =
                                    match unbox lastValue with
                                    | Some (_, b) -> b
                                    | _ -> null

                                if string _lastValue = string newValue then
                                    (Logger.consoleError
                                        $"should have skipped assign
        lastValue={lastValue} typeof _lastValue={jsTypeof _lastValue} newValue={newValue} typeof newValue={jsTypeof newValue} ticks={ticks} {getDebugInfo ()}")

                                $"gun.on() value. triggering. ##
        lastValue={lastValue} typeof _lastValue={jsTypeof _lastValue} newValue={newValue} typeof newValue={jsTypeof newValue} ticks={ticks} {getDebugInfo ()}")

                        //                        Browser.Dom.window?atomPath <- atomPath
                        //                        Browser.Dom.window?lastValue <- _lastValue
                        //                        Browser.Dom.window?newValue <- newValue
                        //                        Browser.Dom.window?deepEqual <- Object.compare

                        // setAtom internalAtom

                        Gun.batchData setAtom (ticks, newValue)
                    with
                    | ex ->
                        Logger.consoleError ("[exception8]", ex, newValue)
                        raise ex
        with
        | ex ->
            Logger.consoleError ("[exception1]", ex, newValue)
            onError ()

    let inline syncSubscribe
        getDebugInfo
        (syncEngine: SyncEngine)
        (syncState: SyncState<'TValue>)
        (trigger: TicksGuid * AdapterValue<'TValue> option -> unit)
        onError
        atomPath
        =
        promise {
            match syncEngine.GetGunAtomNode (), syncState.GunSubscription with
            | _, Some _ ->
                Logger.logTrace
                    (fun () -> $"[syncSubscribe] skipping subscribe, lastSubscription is set. {getDebugInfo ()} ")
            | Some (key, gunAtomNode), None ->
                let gunKeys =
                    let user = gunAtomNode.user ()
                    user.__.sea

                Profiling.addCount $"{key} subscribe"

                Logger.logTrace (fun () -> $"[syncSubscribe] batch subscribing. key={key} {getDebugInfo ()} ")

                //                    gunAtomNode.off () |> ignore

                match gunKeys with
                | Some gunKeys ->
                    match syncEngine.GetHub () with
                    | Some hub ->
                        promise {
                            try
                                match syncState.HubSubscription, syncEngine.GetAlias () with
                                | Some _, _ -> Logger.logError (fun () -> $"sub already present key={key}")
                                | None, None -> Logger.consoleError "alias is none (subscription)"
                                | None, Some (Gun.Alias alias) ->

                                    let subscription =
                                        Gun.batchHubSubscribe
                                            hub
                                            (Sync.Request.Get (alias, atomPath |> AtomPath.Value))
                                            (Guid.newTicksGuid ())
                                            (fun (ticks, msg: Sync.Response) ->
                                                Logger.logTrace
                                                    (fun () ->
                                                        $"[syncSubscribe] wrapper.next() HUB stream subscribe] msg={msg} {getDebugInfo ()} ")

                                                promise {
                                                    match msg with
                                                    | Sync.Response.GetResult result ->
                                                        Logger.logTrace
                                                            (fun () ->
                                                                $"[syncSubscribe] Sync.Response.GetResult key={key} atomPath={atomPath} {getDebugInfo ()} ")

                                                        let! newValue =
                                                            match result |> Option.defaultValue null with
                                                            | null -> unbox null |> Promise.lift
                                                            | result ->
                                                                Gun.userDecode<'TValue>
                                                                    gunKeys
                                                                    (Gun.EncryptedSignedValue result)

                                                        trigger (ticks, (newValue |> Option.map AdapterValue.Hub))
                                                    | _ -> ()

                                                    return! newHashedDisposable ticks
                                                })
                                            (fun ex ->
                                                Logger.logError
                                                    (fun () -> $"[syncSubscribe] onError... ex={ex} {getDebugInfo ()} ")

                                                onError ())

                                    syncState.HubSubscription <- Some subscription
                            with
                            | ex -> Logger.consoleError $"hub.get, setInternalFromGun, error={ex.Message}"
                        }
                        |> Promise.start
                    | None -> Logger.logTrace (fun () -> $"[syncSubscribe] skipping...{getDebugInfo ()} ")

                    match syncEngine.GetGunOptions (), syncEngine.GetAtomPath () with
                    | Some (GunOptions.Sync _), Some (AtomPath _atomPath) ->
                        //                                if false then
                        Gun.batchSubscribe
                            gunAtomNode
                            (Guid.newTicksGuid ())
                            (fun (ticks, gunValue) ->
                                promise {
                                    let data =
                                        match gunValue with
                                        | Gun.GunValue.EncryptedSignedValue (Gun.EncryptedSignedValue result) -> result
                                        | _ -> null

                                    let! newValue =
                                        match gunValue with
                                        | Gun.GunValue.EncryptedSignedValue result ->
                                            Gun.userDecode<'TValue> gunKeys result
                                        | _ -> unbox null |> Promise.lift

                                    trigger (ticks, (newValue |> Option.map AdapterValue.Gun))

                                    let hubValue =
                                        match syncState.AdapterValueMapByType with
                                        | Some adapterValueMapByType ->
                                            adapterValueMapByType
                                            |> Map.tryFind AdapterType.Hub
                                        | None -> None

                                    if hubValue.IsNone
                                       || hubValue |> Object.compare newValue then
                                        Logger.logTrace
                                            (fun () ->
                                                $"debouncedPut() HUB (update from gun) SKIPPED
                                                newValue={newValue} jsTypeof-newValue={jsTypeof newValue} {getDebugInfo ()}")
                                    else
                                        match syncEngine.GetAtomPath (), syncEngine.GetHub (), syncEngine.GetAlias () with
                                        | Some (AtomPath atomPath), Some hub, Some (Gun.Alias alias) ->
                                            promise {
                                                try
                                                    let! response =
                                                        hub.invokeAsPromise (Sync.Request.Set (alias, atomPath, data))

                                                    match response with
                                                    | Sync.Response.SetResult result ->
                                                        if not result then
                                                            Logger.consoleError "$$$$ HUB PUT ERROR (backend console)"
                                                        else
                                                            Logger.logTrace
                                                                (fun () ->
                                                                    $"subscribe() hub set from gun
                                    newValue={newValue} jsTypeof-newValue={jsTypeof newValue} {getDebugInfo ()}")

                                                            trigger (ticks, (newValue |> Option.map AdapterValue.Hub))
                                                    | response -> Logger.consoleError ("#00002 response:", response)
                                                with
                                                | ex -> Logger.consoleError $"$$$$ hub.set, error={ex.Message}"
                                            }
                                            |> Promise.start
                                        | _ ->
                                            Logger.logTrace
                                                (fun () ->
                                                    $"[$$$$ wrapper.on() HUB put] skipping. newValue={newValue}. {getDebugInfo ()} ")

                                    return! newHashedDisposable ticks
                                })

                        syncState.GunSubscription <- Some DateTime.Now.Ticks
                    | _ -> Logger.logTrace (fun () -> $"[syncSubscribe] skipping. {getDebugInfo ()} ")
                | _ -> Logger.logTrace (fun () -> $"[syncSubscribe] skipping. gun keys empty {getDebugInfo ()} ")



            | None, _ ->
                Logger.logTrace
                    (fun () ->
                        $"[syncSubscribe] skipping subscribe, no gun atom node. (maybe no alias) {getDebugInfo ()} ")
        }


    let inline putFromUi<'TValue>
        getDebugInfo
        (syncEngine: SyncEngine)
        (syncTrigger: TicksGuid * AdapterValue<'TValue> option -> unit)
        (ticks, newValue)
        =
        promise {
            let syncState = SyncState<'TValue> ()

            Logger.logTrace (fun () -> $"atomFamily.wrapper.set() debounceGunPut promise. #1 newValue={newValue}")

            try
                match syncEngine.GetGunAtomNode () with
                | Some (key, gunAtomNode) ->
                    let user = gunAtomNode.user ()
                    let keys = user.__.sea

                    match keys with
                    | Some keys ->
                        Logger.logTrace
                            (fun () ->
                                $"atomFamily.wrapper.set() debounceGunPut promise. #2 before encode {key} newValue={newValue}")

                        let! newValueJson =
                            promise {
                                if newValue |> Js.ofNonEmptyObj |> Option.isNone then
                                    return null
                                else
                                    let! (Gun.EncryptedSignedValue encrypted) = Gun.userEncode<'TValue> keys newValue

                                    return encrypted
                            }

                        Logger.logTrace
                            (fun () ->
                                $"atomFamily.wrapper.set() debounceGunPut promise. #3.
    before put {key} newValue={newValue} {getDebugInfo ()}")

                        let hubValue =
                            match syncState.AdapterValueMapByType with
                            | Some adapterValueMapByType ->
                                adapterValueMapByType
                                |> Map.tryFind AdapterType.Hub
                            | None -> None

                        match hubValue with
                        | Some lastHubValue when
                            lastHubValue |> Object.compare newValue
                            || unbox lastHubValue = null
                            ->
                            Logger.logTrace
                                (fun () ->
                                    $"debouncedPut() HUB SKIPPED
    newValue={newValue} jsTypeof-newValue={jsTypeof newValue} {getDebugInfo ()}")
                        | _ ->
                            match syncEngine.GetAtomPath (), syncEngine.GetHub (), syncEngine.GetAlias () with
                            | Some (AtomPath atomPath), Some hub, Some (Gun.Alias alias) ->
                                promise {
                                    try
                                        let! response =
                                            hub.invokeAsPromise (Sync.Request.Set (alias, atomPath, newValueJson))

                                        match response with
                                        | Sync.Response.SetResult result ->
                                            if not result then
                                                Logger.consoleError "HUB PUT ERROR (backend console)"
                                            else
                                                syncTrigger (ticks, Some (AdapterValue.Hub newValue))
                                        | response -> Logger.consoleError ("#90592 response:", response)
                                    with
                                    | ex -> Logger.consoleError $"hub.set, error={ex.Message}"
                                }
                                |> Promise.start
                            | _ ->
                                Logger.logTrace
                                    (fun () ->
                                        $"[wrapper.on() HUB put]. skipping. newValue={newValue} {getDebugInfo ()} ")

                        let gunValue =
                            match syncState.AdapterValueMapByType with
                            | Some adapterValueMapByType ->
                                adapterValueMapByType
                                |> Map.tryFind AdapterType.Gun
                            | None -> None

                        match syncEngine.GetGunOptions () with
                        | Some (GunOptions.Sync _) when gunValue |> Object.compare (Some newValue) |> not ->
                            if gunValue.IsNone
                               || gunValue |> Object.compare newValue |> not
                               || unbox newValue = null then

                                let! putResult =
                                    Gun.put
                                        gunAtomNode
                                        (Gun.GunValue.EncryptedSignedValue (Gun.EncryptedSignedValue newValueJson))

                                if putResult then
                                    syncTrigger (ticks, Some (AdapterValue.Gun newValue))

                                    Logger.logTrace
                                        (fun () ->
                                            $"atomFamily.wrapper.set() debounceGunPut promise result. newValue={newValue} {key} {getDebugInfo ()} ")
                                else
                                    Browser.Dom.window?lastPutResult <- putResult

                                    match Dom.window () with
                                    | Some window ->
                                        if window?Cypress = null then
                                            Logger.consoleError
                                                $"atomFamily.wrapper.set() debounceGunPut promise put error. newValue={newValue} putResult={putResult} {key} {getDebugInfo ()}"
                                    | None -> ()
                        | _ ->
                            Logger.logTrace
                                (fun () ->
                                    $"debouncedPut() SKIPPED newValue={newValue} jsTypeof-newValue={jsTypeof newValue} {getDebugInfo ()}")
                    | None -> Logger.logTrace (fun () -> $"atomFamily.wrapper.set(). skipped ...")


                | None ->
                    Logger.logTrace
                        (fun () ->
                            $"<filter> [gunEffect.debounceGunPut promise] skipping gun put. no gun atom node. newValue={newValue} {getDebugInfo ()}")
            with
            | ex -> Logger.consoleError ("[exception2]", ex, newValue)

            syncState.SyncPaused <- false

            return! newHashedDisposable ticks
        }

    let inline groupAdapterValueMapByType adapterValueMap =
        let newMap =
            adapterValueMap
            |> Map.toSeq
            |> Seq.map
                (function
                | ticks, AdapterValue.Internal value -> AdapterType.Internal, (ticks, value)
                | ticks, AdapterValue.Gun value -> AdapterType.Gun, (ticks, value)
                | ticks, AdapterValue.Hub value -> AdapterType.Hub, (ticks, value))
            |> Seq.groupBy fst
            |> Map.ofSeq
            |> Map.map
                (fun _ v ->
                    v
                    |> Seq.map snd
                    |> Seq.sortByDescending fst
                    |> Seq.head)

        Reflection.unionCases<AdapterType>
        |> List.map (fun case -> case, (newMap |> Map.tryFind case))
        |> Map.ofList

    let inline atomWithSync<'TKey, 'TValue> atomKey (defaultValue: 'TValue) =
        let mutable lastUserAtomId = None

        let syncEngine = SyncEngine None
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
                        oldAdapterValueMap
                        |> (match newValue with
                            | Some value -> Map.add ticks value
                            | None -> Map.remove ticks))
            | None -> ()

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

                            setInternalFromSync
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
                                syncSubscribe getDebugInfo syncEngine syncState trigger onError atomPath
                                |> Promise.start)
                            1000
                        |> ignore

                    syncSubscribe getDebugInfo syncEngine syncState trigger onError atomPath
                    |> Promise.start)
                100

        let batchPutFromUi (ticks, newValue) =
            Batcher.batch (
                Batcher.BatchType.Set (
                    ticks,
                    (fun ticks -> putFromUi getDebugInfo syncEngine syncTrigger (ticks, newValue))
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
                            let newValue =
                                match jsTypeof newValueFn with
                                | "function" ->
                                    eprintfn $"!!! sync atom change sync. function instead of value. {getDebugInfo ()}"

                                    //                                    let lastSyncValueAtom =
//                                        Store.value getter (lastSyncValueAtom (syncEngine.GetAlias ()))

                                    //                                    (unbox newValueFn) lastSyncValueAtom |> unbox
                                    (unbox newValueFn) (unbox null) |> unbox
                                | _ -> newValueFn

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
                                       syncUnsubscribe
                                           getDebugInfo
                                           (syncEngine.GetGunAtomNode ())
                                           syncState.GunSubscription
                                           (fun () -> syncState.GunSubscription <- None)

        wrapper?init <- defaultValue

        Internal.registerAtom Internal.AtomType.AtomWithStorage atomPath wrapper

        wrapper

    [<RequireQualifiedAccess>]
    type BatchKind =
        | Replace
        | Union

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

        let syncEngine = SyncEngine (Some (fun (key, node) -> key, node.back().back ()))

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
                                let newValue =
                                    match jsTypeof newValueFn with
                                    | "function" -> (unbox newValueFn) oldValue
                                    | _ -> newValueFn

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
                                            $" @@$ atomKeys gun.on() HUB filter fetching/subscribing] @@@ gunAtomNode.map().on result
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
                                $"@@$ atomKeys gun.on() HUB filter fetching/subscribing] @@@ gunAtomNode.map().on skip.
                                  syncEngine.GetGunOptions() not in sync key={key} {getDebugInfo ()} ")

                    Logger.logTrace
                        (fun () ->
                            $"@@ [atomKeys gun.on() HUB filter fetching/subscribing] @@@
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
                                    | true, _sub -> Logger.logError (fun () -> $"sub already present key={key}")
                                    | _ ->
                                        let handle items =
                                            if items |> Array.isEmpty |> not then
                                                Logger.logTrace
                                                    (fun () ->
                                                        $"@@( atomKeys gun.on() HUB filter fetching/subscribing] @@@
                                                                      setting keys locally. items.Length={items.Length}
                                                                      atomPath={atomPath} syncEngine.atomPath={syncEngine.GetAtomPath ()} key={key} {getDebugInfo ()} ")

                                                batchKeysAtom
                                                    (Guid.newTicksGuid (), items |> Array.map onFormat)
                                                    BatchKind.Replace
                                            else
                                                Logger.logTrace
                                                    (fun () ->
                                                        $"@@( atomKeys gun.on() HUB filter fetching/subscribing] @@@
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
                                                        $"@@ [wrapper.next() HUB keys stream subscribe] ticks={ticks} {getDebugInfo ()} response={response}")

                                                promise {
                                                    match response with
                                                    | Sync.Response.FilterResult items ->
                                                        handle items

                                                        Logger.logTrace
                                                            (fun () ->
                                                                $"@@ [wrapper.on() HUB KEYS subscribe] atomPath={atomPath} items={JS.JSON.stringify items} {getDebugInfo ()} ")
                                                    | response ->
                                                        Logger.consoleError (
                                                            "Gun.batchHubSubscribe invalid response:",
                                                            response
                                                        )

                                                    return! newHashedDisposable ticks
                                                })
                                            (fun _ex ->
                                                Selectors.Hub.hubSubscriptionMap.Remove collectionPath
                                                |> ignore)
                                | _ -> Logger.consoleError $"#123561 invalid atom path atomPath={atomPath}"
                            with
                            | ex -> Logger.consoleError $"@@ hub.filter, error={ex.Message}"
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
                        Logger.logTrace (fun () -> $"@@ [wrapper.on() RX KEYS subscribe]  skipping. {getDebugInfo ()}")

                | None, _ ->
                    Logger.logTrace
                        (fun () ->
                            $"@@ [atomKeys gun.on() subscribing] skipping subscribe, no gun atom node. {getDebugInfo ()}")
            }

        let debouncedSubscribe = Js.debounce (subscribe >> Promise.start) 100

        let unsubscribe () =
            match lastSubscription with
            | Some ticks when DateTime.ticksDiff ticks < 1000. ->
                Logger.logTrace
                    (fun () ->
                        $"@@ [atomKeys gun.off()] skipping unsubscribe. jotai resubscribe glitch. {getDebugInfo ()}")
            | Some _ ->
                match syncEngine.GetGunAtomNode () with
                | Some (key, _gunAtomNode) ->

                    Logger.logTrace
                        (fun () ->
                            $"@@  [atomFamily.unsubscribe()] ############ (actually skipped) {key} {getDebugInfo ()} ")

                //                    gunAtomNode.off () |> ignore
//                    lastSubscription <- None
                | None ->
                    Logger.logTrace
                        (fun () ->
                            $"@@  [atomKeys gun.off()] skipping unsubscribe, no gun atom node. {getDebugInfo ()} ")
            | None ->
                Logger.logTrace
                    (fun () ->
                        $"[atomKeys gun.off()] skipping unsubscribe. no last subscription found. {getDebugInfo ()} ")

        wrapper?onMount <- fun (setAtom: 'TKey [] -> unit) ->
                               debouncedSubscribe setAtom
                               fun _ -> unsubscribe ()


        Logger.logTrace
            (fun () ->
                $"Store.selectAtomSyncKeys constructor wrapper={wrapper} lastValue={lastValue} lastSubscription={lastSubscription} {getDebugInfo ()}")

        wrapper?init <- [||]

        splitAtom wrapper


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

    let inline atomWithStorageSync<'TKey, 'TValue> storeRoot name defaultValue =
        let atomKey =
            {
                StoreRoot = storeRoot
                Collection = None
                Keys = []
                Name = name
            }

        let storageAtom = Store.atomWithStorage storeRoot name defaultValue

        let syncAtom = atomWithSync<'TKey, 'TValue> atomKey defaultValue

        let mutable lastSetAtom: ('TValue option -> unit) option = None
        let mutable lastValue = None

        let rec wrapper =
            Store.selector
                storeRoot
                name
                (fun getter ->
                    match Store.value getter syncAtom, Store.value getter storageAtom with
                    | syncValue, storageValue when
                        syncValue |> Object.compare defaultValue
                        && (storageValue |> Object.compare defaultValue
                            || (Store.value getter Selectors.Gun.alias).IsNone
                            || lastValue.IsNone)
                        ->
                        Store.value getter storageAtom
                    | syncValue, _ ->
                        match lastSetAtom with
                        | Some lastSetAtom when
                            lastValue.IsNone
                            || lastValue
                               |> Object.compare (Some syncValue)
                               |> not
                            ->
                            lastValue <- Some syncValue
                            lastSetAtom (Some syncValue)
                        | _ -> ()

                        syncValue)
                (fun _get setter newValue ->
                    if lastValue.IsNone
                       || lastValue |> Object.compare (Some newValue) |> not then
                        lastValue <- Some newValue
                        Store.set setter syncAtom newValue

                    Store.set setter storageAtom newValue)

        wrapper?onMount <- fun (setAtom: 'TValue option -> unit) ->
                               lastSetAtom <- Some setAtom
                               fun () -> lastSetAtom <- None

        wrapper?init <- defaultValue

        Internal.registerAtom Internal.AtomType.AtomWithStorageSync (atomKey |> AtomKey.AtomPath) wrapper

        wrapper

    module rec Join =
        let collection = Collection (nameof Join)

        let tempValue =
            let rec tempValue =
                atomFamilyWithSync
                    FsStore.root
                    collection
                    (nameof tempValue)
                    (fun (_atomPathGuidHash: Guid) -> null: string)
                    (fun (atomPathGuidHash: Guid) ->
                        [
                            string atomPathGuidHash
                        ])

            jotaiUtils.atomFamily
                (fun (AtomPath atomPath) ->

                    let guidHash = Crypto.getTextGuidHash atomPath
                    let atom = tempValue guidHash

                    Logger.logTrace (fun () -> $"tempValueWrapper constructor. atomPath={atomPath} guidHash={guidHash}")

                    let wrapper =
                        jotai.atom (
                            (fun getter ->
                                let value = Store.value getter atom
                                Profiling.addCount $"{atomPath} tempValue set"

                                Logger.logTrace
                                    (fun () ->
                                        $"tempValueWrapper.get(). atomPath={atomPath} guidHash={guidHash} value={value}")

                                match value with
                                | null -> null
                                | _ ->
                                    match Json.decode<string * string option> value with
                                    | _, Some value -> value
                                    | _ -> null),
                            Some
                                (fun _ setter newValue ->
                                    Profiling.addCount $"{atomPath} tempValue set"

                                    Logger.logTrace
                                        (fun () ->
                                            $"tempValueWrapper.set(). atomPath={atomPath} guidHash={guidHash} newValue={newValue}")

                                    let newValue = Json.encode (atomPath, newValue |> Option.ofObj)

                                    Logger.logTrace (fun () -> $"tempValueWrapper.set(). newValue2={newValue} ")

                                    Store.set setter atom (newValue |> box |> unbox))
                        )

                    wrapper)
                Object.compare


    let provider = jotai.provider

    let emptyAtom = jotai.atom<obj> null
    let emptyArrayAtom = jotai.atom<obj []> [||]

    let inline waitForAll<'T> (atoms: Atom<'T> []) =
        match atoms with
        | [||] -> unbox emptyArrayAtom
        | _ -> jotaiUtils.waitForAll atoms


    let inline getAtomField (atom: InputAtom<'TValue> option) (inputScope: AtomScope) =
        match atom with
        | Some (InputAtom atomPath) ->
            {
                Current =
                    match atomPath with
                    | AtomReference.Atom atom -> Some atom
                    | _ -> Some (unbox emptyAtom)
                Temp =
                    //                    Dom.log
//                        (fun () -> $"getAtomField atomPath={atomPath} queryAtomPath atomPath={queryAtomPath atomPath}")

                    match Internal.queryAtomPath atomPath, inputScope with
                    | Some atomPath, AtomScope.Temp -> Some (Join.tempValue atomPath)
                    | _ -> None
            }
        | _ -> { Current = None; Temp = None }


    let inline setTempValue<'TValue9, 'TKey> (setter: SetFn) (atom: Atom<'TValue9>) (value: 'TValue9) =
        let atomField = getAtomField (Some (InputAtom (AtomReference.Atom atom))) AtomScope.Temp

        match atomField.Temp with
        | Some atom -> Store.set setter atom (value |> Json.encode<'TValue9>)
        | _ -> ()

    let inline scopedSet<'TValue10, 'TKey>
        (setter: SetFn)
        (atomScope: AtomScope)
        (atom: 'TKey -> Atom<'TValue10>, key: 'TKey, value: 'TValue10)
        =
        match atomScope with
        | AtomScope.Current -> Store.set setter (atom key) value
        | AtomScope.Temp -> setTempValue<'TValue10, 'TKey> setter (atom key) value

    let inline resetTempValue<'TValue8, 'TKey> (setter: SetFn) (atom: Atom<'TValue8>) =
        let atomField = getAtomField (Some (InputAtom (AtomReference.Atom atom))) AtomScope.Temp

        match atomField.Temp with
        | Some atom -> Store.set setter atom null
        | _ -> ()

    let rec ___emptyTempAtom = nameof ___emptyTempAtom

    let inline getTempValue<'TValue11, 'TKey> getter (atom: Atom<'TValue11>) =
        let atomField = getAtomField (Some (InputAtom (AtomReference.Atom atom))) AtomScope.Temp

        match atomField.Temp with
        | Some tempAtom ->
            let result = Store.value getter tempAtom

            match result with
            | result when result = ___emptyTempAtom -> unbox null
            | null -> Store.value getter atom
            | _ -> Json.decode<'TValue11> result
        | _ -> Store.value getter atom

    let inline deleteRoot getter atom =
        promise {
            let alias = Store.value getter Selectors.Gun.alias
            let atomPath = Internal.queryAtomPath (AtomReference.Atom atom)

            let gunAtomNode = gunAtomNodeFromAtomPath getter alias atomPath

            match gunAtomNode with
            | Some (_key, gunAtomNode) ->
                let! _putResult = Gun.put (gunAtomNode.back ()) (unbox null)
                ()
            | None -> ()

            match alias, atomPath with
            | Some (Gun.Alias alias), Some (AtomPath atomPath) ->
                let hub = Store.value getter Selectors.Hub.hub

                match hub with
                | Some hub ->
                    let nodes = atomPath |> String.split "/"

                    if nodes.Length > 3 then
                        let rootAtomPath = nodes |> Array.take 3 |> String.concat "/"
                        do! hub.sendAsPromise (Sync.Request.Set (alias, rootAtomPath, null))
                | _ -> ()
            | _ -> ()
        }

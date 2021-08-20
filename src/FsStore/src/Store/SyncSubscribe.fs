namespace FsStore.Store

open Fable.Core.JsInterop
open System
open FsStore.BaseStore.Store
open FsStore.Model
open FsBeacon.Shared
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs
open FsStore.Bindings


[<AutoOpen>]
module SyncSubscribe =
    module Store =
        let inline syncSubscribe
            getDebugInfo
            (syncEngine: Store.SyncEngine<_>)
            (syncState: SyncState<'TValue>)
            (trigger: TicksGuid * AdapterValue<'TValue> option -> unit)
            onError
            atomPath
            =
            promise {
                match syncEngine.GetGunAtomNode (), syncState.GunSubscription with
                | _, Some _ ->
                    Logger.logTrace
                        (fun () ->
                            $"Store.syncSubscribe. skipping subscribe, lastSubscription is set. {getDebugInfo ()} ")
                | Some (key, gunAtomNode), None ->
                    let gunKeys =
                        let user = gunAtomNode.user ()
                        user.__.sea

                    Profiling.addCount $"{key} subscribe"

                    Logger.logTrace (fun () -> $"Store.syncSubscribe. batch subscribing. key={key} {getDebugInfo ()} ")

                    //                    gunAtomNode.off () |> ignore

                    match gunKeys with
                    | Some gunKeys ->
                        match syncEngine.GetHub () with
                        | Some hub ->
                            promise {
                                try
                                    match syncState.HubSubscription, syncEngine.GetAlias () with
                                    | Some _, _ ->
                                        Logger.logError
                                            (fun () -> $"Store.syncSubscribe. sub already present key={key}")
                                    | None, None ->
                                        Logger.logError (fun () -> "Store.syncSubscribe. alias is none (subscription)")
                                    | None, Some (Gun.Alias alias) ->

                                        let subscription =
                                            Gun.batchHubSubscribe
                                                hub
                                                (Sync.Request.Get (alias, atomPath |> AtomPath.Value))
                                                (Guid.newTicksGuid ())
                                                (fun (ticks, msg: Sync.Response) ->
                                                    Logger.logTrace
                                                        (fun () ->
                                                            $"Store.syncSubscribe. wrapper.next() HUB stream subscribe] msg={msg} {getDebugInfo ()} ")

                                                    promise {
                                                        match msg with
                                                        | Sync.Response.GetResult result ->
                                                            Logger.logTrace
                                                                (fun () ->
                                                                    $"Store.syncSubscribe. Sync.Response.GetResult key={key} atomPath={atomPath} {getDebugInfo ()} ")

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
                                                        (fun () ->
                                                            $"Store.syncSubscribe. onError... ex={ex} {getDebugInfo ()} ")

                                                    onError ())

                                        syncState.HubSubscription <- Some subscription
                                with
                                | ex ->
                                    Logger.logError
                                        (fun () ->
                                            $"Store.syncSubscribe. hub.get, setInternalFromGun, ex.Message={ex.Message} ex={ex}")
                            }
                            |> Promise.start
                        | None -> Logger.logTrace (fun () -> $"Store.syncSubscribe. skipping...{getDebugInfo ()} ")

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
                                            | Gun.GunValue.EncryptedSignedValue (Gun.EncryptedSignedValue result) ->
                                                result
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
                                                    $"Store.syncSubscribe. debouncedPut() HUB (update from gun) SKIPPED
                                                    newValue={newValue} jsTypeof-newValue={jsTypeof newValue} {getDebugInfo ()}")
                                        else
                                            match syncEngine.GetAtomPath (),
                                                  syncEngine.GetHub (),
                                                  syncEngine.GetAlias () with
                                            | Some (AtomPath atomPath), Some hub, Some (Gun.Alias alias) ->
                                                promise {
                                                    try
                                                        let! response =
                                                            hub.invokeAsPromise (
                                                                Sync.Request.Set (alias, atomPath, data)
                                                            )

                                                        match response with
                                                        | Sync.Response.SetResult result ->
                                                            if not result then
                                                                Logger.logError
                                                                    (fun () ->
                                                                        "Store.syncSubscribe. $$$$ HUB PUT ERROR (backend console)")
                                                            else
                                                                Logger.logTrace
                                                                    (fun () ->
                                                                        $"Store.syncSubscribe. subscribe() hub set from gun
                                        newValue={newValue} jsTypeof-newValue={jsTypeof newValue} {getDebugInfo ()}")

                                                                trigger (
                                                                    ticks,
                                                                    (newValue |> Option.map AdapterValue.Hub)
                                                                )
                                                        | response ->
                                                            Logger.logError
                                                                (fun () ->
                                                                    $"Store.syncSubscribe. #00002 response{response}")
                                                    with
                                                    | ex ->
                                                        Logger.logError
                                                            (fun () ->
                                                                $"Store.syncSubscribe. hub.set, error={ex.Message}")
                                                }
                                                |> Promise.start
                                            | _ ->
                                                Logger.logTrace
                                                    (fun () ->
                                                        $"Store.syncSubscribe. [$$$$ wrapper.on() HUB put] skipping. newValue={newValue}. {getDebugInfo ()} ")

                                        return! newHashedDisposable ticks
                                    })

                            syncState.GunSubscription <- Some DateTime.Now.Ticks
                        | _ -> Logger.logTrace (fun () -> $"Store.syncSubscribe. skipping. {getDebugInfo ()} ")
                    | _ ->
                        Logger.logTrace (fun () -> $"Store.syncSubscribe. skipping. gun keys empty {getDebugInfo ()} ")



                | None, _ ->
                    Logger.logTrace
                        (fun () ->
                            $"Store.syncSubscribe. skipping subscribe, no gun atom node. (maybe no alias) {getDebugInfo ()} ")
            }

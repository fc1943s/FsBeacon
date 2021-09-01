namespace FsStore.Store

open Fable.Core.JsInterop
open System
open FsStore
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
            (trigger: TicksGuid * Atom.AdapterType * 'TValue option -> unit)
            onError
            atomPath
            =
            promise {
                match syncEngine.GetGunAtomNode (), syncState.GunSubscription with
                | _, Some _ ->
                    Logger.logTrace
                        (fun () ->
                            $"Store.syncSubscribe. skipping subscribe, lastSubscription is set. {getDebugInfo ()} ")
                | Some gunAtomNode, None ->
                    let gunKeys =
                        let user = gunAtomNode.user ()
                        user.__.sea

                    Logger.logTrace (fun () -> $"Store.syncSubscribe. batch subscribing. {getDebugInfo ()} ")

                    //                    gunAtomNode.off () |> ignore

                    match gunKeys with
                    | Some gunKeys ->
                        match syncEngine.GetHub () with
                        | Some hub ->
                            promise {
                                try
                                    match syncState.HubSubscription, syncEngine.GetAlias () with
                                    | Some _, _ ->
                                        Logger.logError (fun () -> $"Store.syncSubscribe. sub already present")
                                    | None, None ->
                                        Logger.logError (fun () -> "Store.syncSubscribe. alias is none (subscription)")
                                    | None, Some (Gun.Alias alias) ->

                                        let! subscription =
                                            Gun.hubSubscribe
                                                hub
                                                (Sync.Request.Get (alias, atomPath |> AtomPath.Value))
                                                (fun (msg: Sync.Response) ->
                                                    promise {
                                                        Logger.logTrace
                                                            (fun () ->
                                                                $"Store.syncSubscribe. wrapper.next() HUB stream subscribe] msg={msg} {getDebugInfo ()} ")

                                                        match msg with
                                                        | Sync.Response.GetResult result ->
                                                            Logger.logTrace
                                                                (fun () ->
                                                                    $"Store.syncSubscribe. Sync.Response.GetResult  atomPath={atomPath} {getDebugInfo ()} ")

                                                            let! newValue =
                                                                match result |> Option.defaultValue null with
                                                                | null -> unbox null |> Promise.lift
                                                                | result ->
                                                                    Gun.userDecode<TicksGuid * 'TValue>
                                                                        gunKeys
                                                                        (Gun.EncryptedSignedValue result)

                                                            match newValue with
                                                            | Some (ticks, newValue) ->
                                                                trigger (
                                                                    ticks,
                                                                    Atom.AdapterType.Hub,
                                                                    (newValue |> Option.ofObjUnbox)
                                                                )
                                                            | None ->
                                                                failwith
                                                                    $"invalid data: newValue={newValue} result={result}"
                                                        | _ -> ()
                                                    }
                                                    |> Promise.start)
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
                                (fun (ticks, gunValue, _key) ->
                                    promise {
                                        let data =
                                            match gunValue with
                                            | Gun.EncryptedSignedValue result -> result

                                        let! newValue = Gun.userDecode<'TValue> gunKeys gunValue

                                        trigger (ticks, Atom.AdapterType.Gun, newValue)

                                        let hubValue =
                                            match syncState.AdapterValueMapByType with
                                            | Some adapterValueMapByType ->
                                                adapterValueMapByType
                                                |> Map.tryFind Atom.AdapterType.Hub
                                            | None -> None

                                        if hubValue.IsNone
                                           || hubValue.Value
                                              |> Option.map snd
                                              |> Object.compare newValue then
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

                                                                trigger (ticks, Atom.AdapterType.Hub, newValue)
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

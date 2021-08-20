namespace FsStore.Store

open Fable.Core.JsInterop
open FsStore.BaseStore.Store
open FsStore.Model
open FsBeacon.Shared
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs
open FsStore.Bindings


[<AutoOpen>]
module PutFromUi =
    module Store =
        let inline putFromUi<'TValue>
            (getDebugInfo: unit -> string)
            (syncEngine: Store.SyncEngine)
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
                                        let! (Gun.EncryptedSignedValue encrypted) =
                                            Gun.userEncode<'TValue> keys newValue

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

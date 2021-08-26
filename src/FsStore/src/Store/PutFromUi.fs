namespace FsStore.Store

open Fable.Core.JsInterop
open FsStore.Atom
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
            (syncEngine: Store.SyncEngine<'TValue>)
            (syncTrigger: TicksGuid * AdapterType * 'TValue option -> unit)
            (ticks, newValue)
            =
            promise {
                let syncState = SyncState<'TValue> ()

                Logger.logTrace
                    (fun () ->
                        $"Store.putFromUi. atomFamily.wrapper.set() debounceGunPut promise. #1 newValue={newValue}")

                try
                    match syncEngine.GetGunAtomNode () with
                    | Some gunAtomNode ->
                        let user = gunAtomNode.user ()
                        let keys = user.__.sea

                        match keys with
                        | Some keys ->
                            Logger.logTrace
                                (fun () ->
                                    $"Store.putFromUi. atomFamily.wrapper.set() debounceGunPut promise. #2 before encode newValue={newValue}")

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
                                    $"Store.putFromUi. atomFamily.wrapper.set() debounceGunPut promise. #3.
        before put newValue={newValue} {getDebugInfo ()}")

                            let hubValue =
                                match syncState.AdapterValueMapByType with
                                | Some adapterValueMapByType ->
                                    adapterValueMapByType
                                    |> Map.tryFind AdapterType.Hub
                                | None -> None

                            match hubValue with
                            | Some (Some lastHubValue) when
                                lastHubValue |> snd |> Object.compare newValue
                                || unbox lastHubValue = null
                                ->
                                Logger.logTrace
                                    (fun () ->
                                        $"Store.putFromUi. debouncedPut() HUB SKIPPED
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
                                                    Logger.logError
                                                        (fun () -> "Store.putFromUi. HUB PUT ERROR (backend console)")
                                                else
                                                    syncTrigger (ticks, AdapterType.Hub, Some newValue)
                                            | response ->
                                                Logger.logError
                                                    (fun () -> $"Store.putFromUi. #90592 response={response}")
                                        with
                                        | ex ->
                                            Logger.logError (fun () -> $"Store.putFromUi. hub.set, error={ex.Message}")
                                    }
                                    |> Promise.start
                                | _ ->
                                    Logger.logTrace
                                        (fun () ->
                                            $"Store.putFromUi. [wrapper.on() HUB put]. skipping. newValue={newValue} {getDebugInfo ()} ")

                            let gunValue =
                                match syncState.AdapterValueMapByType with
                                | Some adapterValueMapByType ->
                                    adapterValueMapByType
                                    |> Map.tryFind AdapterType.Gun
                                | None -> None

                            match syncEngine.GetGunOptions () with
                            | Some (GunOptions.Sync _) when
                                gunValue.Value
                                |> Option.map snd
                                |> Object.compare (Some newValue)
                                |> not
                                ->
                                if gunValue.IsNone
                                   || gunValue.Value
                                      |> Option.map snd
                                      |> Object.compare (Some newValue)
                                      |> not
                                   || unbox newValue = null then

                                    let! putResult =
                                        Gun.put
                                            gunAtomNode
                                            (Gun.GunValue.EncryptedSignedValue (Gun.EncryptedSignedValue newValueJson))

                                    if putResult then
                                        syncTrigger (ticks, AdapterType.Gun, Some newValue)

                                        Logger.logTrace
                                            (fun () ->
                                                $"Store.putFromUi. atomFamily.wrapper.set() debounceGunPut promise result. newValue={newValue} {getDebugInfo ()} ")
                                    else
                                        Browser.Dom.window?lastPutResult <- putResult

                                        match Dom.window () with
                                        | Some window ->
                                            if window?Cypress = null then
                                                Logger.logError
                                                    (fun () ->
                                                        $"Store.putFromUi. atomFamily.wrapper.set() debounceGunPut promise put error. newValue={newValue} putResult={putResult} {getDebugInfo ()}")
                                        | None -> ()
                            | _ ->
                                Logger.logTrace
                                    (fun () ->
                                        $"Store.putFromUi. debouncedPut() SKIPPED newValue={newValue} jsTypeof-newValue={jsTypeof newValue} {getDebugInfo ()}")
                        | None -> Logger.logTrace (fun () -> $"atomFamily.wrapper.set(). skipped ...")


                    | None ->
                        Logger.logTrace
                            (fun () ->
                                $"<filter> Store.putFromUi. [gunEffect.debounceGunPut promise] skipping gun put. no gun atom node. newValue={newValue} {getDebugInfo ()}")
                with
                | ex -> Logger.logError (fun () -> $"Store.putFromUi. ex={ex} newValue={newValue}")

                syncState.SyncPaused <- false

                return! newHashedDisposable ticks
            }

namespace FsStore.Store

open Fable.Core.JsInterop
open Fable.Core
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs
open FsStore.Bindings


[<AutoOpen>]
module SetInternalFromSync =
    module Store =
        let inline setInternalFromSync getDebugInfo setAtom syncPaused lastValue onError (ticks, newValue, key) =
            try
                Logger.logTrace
                    (fun () ->
                        $"Store.setInternalFromSync. gun.on() value. start.
    newValue={newValue} jsTypeof-newValue={jsTypeof newValue} lastValue={lastValue} ticks={ticks} {getDebugInfo ()}")

                match syncPaused, lastValue with
                | true, _ ->
                    Logger.logTrace
                        (fun () ->
                            $"Store.setInternalFromSync. gun.on() value. skipping. Sync paused.
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
                            $"Store.setInternalFromSync. gun.on() value. skipping.
    newValue={newValue} jsTypeof-newValue={jsTypeof newValue} lastValue={lastValue} ticks={ticks} {getDebugInfo ()} ")
                | _ ->
                    if unbox newValue = JS.undefined then
                        Logger.logTrace
                            (fun () ->
                                $"Store.setInternalFromSync. gun.on() value. skipping. newValue=undefined
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
                                        (Logger.logError
                                            (fun () ->
                                                $"Store.setInternalFromSync. should have skipped assign
            lastValue={lastValue} typeof _lastValue={jsTypeof _lastValue} newValue={newValue} typeof newValue={jsTypeof newValue} ticks={ticks} {getDebugInfo ()}"))

                                    $"Store.setInternalFromSync. gun.on() value. triggering. ##
            lastValue={lastValue} typeof _lastValue={jsTypeof _lastValue} newValue={newValue} typeof newValue={jsTypeof newValue} ticks={ticks} {getDebugInfo ()}")

                            //                        Browser.Dom.window?atomPath <- atomPath
                            //                        Browser.Dom.window?lastValue <- _lastValue
                            //                        Browser.Dom.window?newValue <- newValue
                            //                        Browser.Dom.window?deepEqual <- Object.compare

                            // setAtom internalAtom

                            Gun.batchData setAtom (ticks, newValue, key)
                        with
                        | ex ->
                            Logger.logError (fun () -> $"Store.setInternalFromSync. ex-inner={ex} newValue={newValue}")
                            raise ex
            with
            | ex ->
                Logger.logError (fun () -> $"Store.setInternalFromSync. ex-outer={ex} newValue={newValue}")
                onError ()

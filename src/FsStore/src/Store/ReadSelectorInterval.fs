namespace FsStore.Store

open Fable.Core.JsInterop
open Fable.Core
open FsStore
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs
open FsStore.Bindings.Jotai


[<AutoOpen>]
module ReadSelectorInterval =
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

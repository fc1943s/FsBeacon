namespace FsUi.Hooks

open FsCore
open Fable.Core
open Feliz
open FsStore.Hooks
open FsStore
open FsStore.Model


module Scheduling =
    [<Erase>]
    type SchedulingType =
        | Timeout
        | Interval

    let inline schedulingFn schedulingType =
        match schedulingType with
        | Timeout -> JS.setTimeout, JS.clearTimeout
        | Interval -> JS.setInterval, JS.clearInterval

    let inline useScheduling schedulingType duration (fn: GetFn -> SetFn -> JS.Promise<unit>) =
        let fnCallback = React.useCallbackRef (fun (getter, setter) -> fn getter setter)

        let savedCallback = React.useRef fnCallback

        React.useEffect (
            (fun () -> savedCallback.current <- fnCallback),
            [|
                box savedCallback
                box fnCallback
            |]
        )

        let isMountedRef = React.useIsMountedRef ()

        let fn =
            Store.useCallbackRef
                (fun getter setter _ ->
                    promise {
                        if isMountedRef.current then
                            do! savedCallback.current (getter, setter)
                    })

        let setFn, clearFn =
            React.useMemo (
                (fun () -> schedulingFn schedulingType),
                [|
                    box schedulingType
                |]
            )

        React.useEffect (
            (fun () ->
                let id = setFn (fn >> Promise.start) duration
                Object.newDisposable (fun () -> clearFn id)),
            [|
                box clearFn
                box setFn
                box fn
                box isMountedRef
                box schedulingType
                box savedCallback
                box duration
            |]
        )

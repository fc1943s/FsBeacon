namespace FsUi.Components

open Fable.Core.JsInterop
open Fable.React
open Feliz
open FsJs
open FsStore
open FsUi.Bindings
open FsUi.Hooks


module DebugPanel =

    [<RequireQualifiedAccess>]
    type DebugPanelDisplay =
        | None
        | Overlay
        | Inline

    let inline mapDict dict =
        dict
        |> Seq.indexed
        |> Seq.map (fun (i, KeyValue (k, v)) -> $"<{i}> {k}", v |> string |> box)

    [<ReactComponent>]
    let DebugPanel display =
        let text, setText = React.useState ""
        let oldJson, setOldJson = React.useState ""
        let showDebug = Store.useValue Atoms.showDebug

        let deviceInfo = Store.useValue Selectors.deviceInfo
        let logger = Store.useValue Selectors.logger

        logger.Info (fun () -> $"DebugPanel.render. showDebug={showDebug}")

        Scheduling.useScheduling
            Scheduling.Interval
            1000
            (fun _ _ ->
                promise {
                    if not showDebug then
                        ()
                    else
                        let json =
                            Json.encodeWithNullFormatted
                                {|
                                    DeviceInfo = deviceInfo
                                    SortedCallCount =
                                        Profiling.profilingState.CallCount
                                        |> mapDict
                                        |> Seq.sortBy fst
                                        |> createObj
                                    CallCount =
                                        Profiling.profilingState.CallCount
                                        |> mapDict
                                        |> createObj
                                    Timestamps =
                                        Profiling.profilingState.Timestamps
                                        |> Seq.map (fun (k, v) -> $"{k} = {v}")
                                        |> Seq.toArray
                                |}

                        if json = oldJson then
                            ()
                        else
                            setText json
                            setOldJson json
                })

        React.fragment [
            //            if debug then
//                Chakra.box
//                    (fun x ->
//                        x.id <- "test1"
//                        x.position <- "absolute"
//                        x.width <- "100px"
//                        x.height <- "80px"
//                        x.top <- "40px"
//                        x.right <- "24px"
//                        x.backgroundColor <- "#ccc3"
//                        x.zIndex <- 1)
//                    [
//                        str "test1"
//                    ]

            Ui.flex
                (fun x ->
                    match display with
                    | DebugPanelDisplay.Overlay ->
                        x.width <- "min-content"
                        x.height <- if showDebug then "60%" else "initial"
                        x.position <- "fixed"
                        x.right <- "24px"
                        x.bottom <- "0"
                        x.zIndex <- 4
                        x.overflow <- if showDebug then "scroll" else "initial"
                    | _ -> ()

                    x.flex <- "1"
                    x.fontSize <- "9px"
                    x.overflow <- "auto"
                    x.flexBasis <- 0
                    x.backgroundColor <- "#44444455")
                [
                    if showDebug then
                        Ui.flex
                            (fun x ->
                                x.flex <- "1"
                                x.id <- "debug"
                                x.whiteSpace <- "pre"
                                x.fontFamily <- "Roboto Condensed Light, system-ui, sans-serif")
                            [
                                str text
                            ]
                ]
        ]

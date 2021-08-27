namespace FsUi.Components

open System
open Fable.Core.JsInterop
open Fable.React
open Feliz
open FsJs
open FsStore
open FsStore.Hooks
open FsUi.Bindings
open FsUi.Hooks
open FsCore
open FsStore.State
open FsUi.State


module DebugPanel =

    [<RequireQualifiedAccess>]
    type DebugPanelDisplay =
        | None
        | Overlay
        | Inline

    let inline mapDict dict =
        dict
        |> Seq.indexed
        |> Seq.map (fun (i, KeyValue (k, v)) -> $"{i}. {k}", box $"""{v} call{if v = 1 then "" else "s"}""")

    [<ReactComponent>]
    let ValueIndicator name atom =
        let value = Store.useValue atom

        Ui.box
            (fun _ -> ())
            [
                str $"[{name}=%A{Json.encodeWithNullFormatted value}]"
            ]

    [<ReactComponent>]
    let GunOptionsIndicator () =
        ValueIndicator (nameof Atoms.gunOptions) Atoms.gunOptions

    [<ReactComponent>]
    let GunPeersIndicator () =
        ValueIndicator (nameof Selectors.Gun.gunPeers) Selectors.Gun.gunPeers

    [<ReactComponent>]
    let HubUrlIndicator () =
        ValueIndicator (nameof Atoms.hubUrl) Atoms.hubUrl

    [<ReactComponent>]
    let UiStateIndicator () =
        ValueIndicator (nameof Selectors.Ui.uiState) Selectors.Ui.uiState

    [<ReactComponent>]
    let SessionRestoredIndicator () =
        ValueIndicator (nameof Atoms.sessionRestored) Atoms.sessionRestored

    [<ReactComponent>]
    let ShowDebugIndicator () =
        ValueIndicator (nameof Atoms.showDebug) Atoms.showDebug

    [<ReactComponent>]
    let AliasIndicator () =
        ValueIndicator (nameof Selectors.Gun.alias) Selectors.Gun.alias

    [<ReactComponent>]
    let PrivateKeysIndicator () =
        ValueIndicator (nameof Selectors.Gun.privateKeys) Selectors.Gun.privateKeys

    let inline getSchedulingInterval (deviceInfo: Dom.DeviceInfo) =
        if not deviceInfo.IsTesting then 1000
        elif Dom.globalExit.Get () then 2000 // * 30
        else 0

    [<ReactComponent>]
    let DebugPanel display =
        let deviceInfo = Store.useValue Selectors.deviceInfo
        let showDebug = Store.useValue Atoms.showDebug
        let interval = (getSchedulingInterval deviceInfo)

        Profiling.addTimestamp
            (fun () -> $"{nameof FsUi} | DebugPanel [ render ] showDebug={showDebug} interval={interval}")

        let text, setText = React.useState ""
        let oldJson, setOldJson = React.useState ""

        Scheduling.useScheduling
            Scheduling.Interval
            interval
            (fun _ _ ->
                promise {
                    if not showDebug then
                        ()
                    else
                        let json =
                            [
                                Json.encodeWithNullFormatted
                                    {|
                                        CountMap =
                                            Profiling.profilingState.CountMap
                                            |> mapDict
                                            |> createObj
                                    |}
                                Json.encodeWithNullFormatted
                                    {|
                                        SortedCountMap =
                                            Profiling.globalProfilingState.Get().CountMap
                                            |> mapDict
                                            |> Seq.sortByDescending (snd >> string)
                                            |> createObj
                                    |}
                                Json.encodeWithNullFormatted
                                    {|
                                        TimestampMap =
                                            Profiling.profilingState.TimestampMap
                                            |> Seq.map (fun (k, v) -> $"{k} = {v}ms")
                                            |> Seq.toArray
                                    |}
                            ]
                            |> String.concat Environment.NewLine

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
                    x.backgroundColor <- "#44444455")
                [
                    if showDebug then
                        Ui.box
                            (fun x ->
                                x.flex <- "1"
                                x.id <- "debug"
                                x.fontSize <- "9px"
                                x.padding <- "4px"
                                x.lineHeight <- "11px"
                                x.whiteSpace <- "pre"
                                x.fontFamily <- "Roboto Condensed Light, system-ui, sans-serif")
                            [
                                AliasIndicator ()
                                PrivateKeysIndicator ()
                                SessionRestoredIndicator ()
                                ShowDebugIndicator ()
                                HubUrlIndicator ()
                                GunOptionsIndicator ()
                                GunPeersIndicator ()
                                UiStateIndicator ()
                                str text
                                ValueIndicator (nameof Selectors.deviceInfo) Selectors.deviceInfo
                            ]
                ]
        ]

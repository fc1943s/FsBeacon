namespace FsBeacon.Template.Components

open FsStore
open Fable.React
open Feliz
open FsJs
open FsStore.Hooks
open FsStore.State
open FsBeacon.Template.State
open FsUi.Bindings
open FsUi.State
open FsUi.Components


module ToggleLogsButton =
    [<ReactComponent>]
    let ToggleLogsButton () =
        let showDebug, setShowDebug = Store.useState Atoms.showDebug
        let logLevel, setLogLevel = Store.useState Atoms.logLevel

        let getLocals () =
            $"showDebug={showDebug} logLevel={logLevel}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | SampleComponent.ToggleLogsButton {fn ()}") getLocals

        Button.Button
            {|
                Tooltip = None
                Icon = Some (Icons.bi.BiRecycle |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <-
                            (fun _ ->
                                promise {
                                    addTimestamp (fun () -> "[ onClick ]") getLocals

                                    setShowDebug (not showDebug)

                                    setLogLevel (
                                        if logLevel = Logger.LogLevel.Trace then
                                            Logger.LogLevel.Debug
                                        else
                                            Logger.LogLevel.Trace
                                    )

                                    Dom.globalDebug.Set (not showDebug)
                                })
                Children =
                    [
                        str $"""{if not showDebug then "enable" else "disable"} logs"""
                    ]
            |}

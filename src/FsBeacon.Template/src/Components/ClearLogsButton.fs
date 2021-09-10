namespace FsBeacon.Template.Components

open FsCore
open Fable.React
open Feliz
open FsJs
open FsBeacon.Template.State
open FsUi.Bindings
open FsUi.Components


module ClearLogsButton =
    [<ReactComponent>]
    let ClearLogsButton () =
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | ClearLogsButton / render") getLocals

        Button.Button
            {|
                Tooltip = None
                Icon = Some (Icons.bi.BiRecycle |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <-
                            (fun _ ->
                                promise {
                                    Profiling.addTimestamp
                                        (fun () -> $"{nameof FsBeacon} | ClearLogsButton [ onClick ]")
                                        getLocals

                                    Profiling.globalClearProfilingState.Get () ()
                                })
                Children =
                    [
                        str "clear logs"
                    ]
            |}

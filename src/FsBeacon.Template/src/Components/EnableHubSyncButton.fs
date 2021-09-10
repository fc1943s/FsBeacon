namespace FsBeacon.Template.Components

open FsStore
open FsCore
open Fable.React
open Feliz
open FsJs
open FsStore.Hooks
open FsBeacon.Template.State
open FsUi.Bindings
open FsUi.State
open FsUi.Components


module EnableHubSyncButton =
    [<ReactComponent>]
    let EnableHubSyncButton () =
        let enableHubSync = Store.useSetState Actions.enableHubSync

        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | EnableHubSyncButton [ render ]") getLocals

        Button.Button
            {|
                Tooltip = None
                Icon = Some (Icons.bi.BiData |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <-
                            (fun _ ->
                                promise {
                                    Profiling.addTimestamp
                                        (fun () -> $"{nameof FsBeacon} | EnableHubSyncButton [ onClick ]")
                                        getLocals

                                    enableHubSync ()
                                })
                Children =
                    [
                        str "enable hub sync"
                    ]
            |}

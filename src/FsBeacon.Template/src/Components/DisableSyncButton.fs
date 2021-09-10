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


module DisableSyncButton =
    [<ReactComponent>]
    let DisableSyncButton () =
        let disableSync = Store.useSetState Actions.disableSync

        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | DisableSyncButton [ render ]") getLocals

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
                                        (fun () -> $"{nameof FsBeacon} | DisableSyncButton [ onClick ]")
                                        getLocals

                                    disableSync ()
                                })
                Children =
                    [
                        str "disable sync"
                    ]
            |}

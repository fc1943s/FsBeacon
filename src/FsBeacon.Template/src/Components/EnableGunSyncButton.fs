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


module EnableGunSyncButton =
    [<ReactComponent>]
    let EnableGunSyncButton () =
        let enableGunSync = Store.useSetState Actions.enableGunSync

        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | EnableGunSyncButton [ render ]") getLocals

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
                                        (fun () -> $"{nameof FsBeacon} | EnableGunSyncButton [ onClick ]")
                                        getLocals

                                    enableGunSync ()
                                })
                Children =
                    [
                        str "enable gun sync"
                    ]
            |}

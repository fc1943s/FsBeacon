namespace FsBeacon.Template.Components

open FsStore
open FsCore
open Fable.React
open Feliz
open FsJs
open FsStore.Hooks
open FsBeacon.Template.State
open FsUi.Bindings
open FsUi.Components


module MountButton =
    [<ReactComponent>]
    let MountButton () =
        let mounted, setMounted = Store.useState Atoms.Sample.mounted

        let getLocals () = $"mounted={mounted} {getLocals ()}"
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | MountButton [ render ]") getLocals

        Button.Button
            {|
                Tooltip = Some (str "Tooltip test")
                Icon = Some (Icons.io5.IoRefreshCircle |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <-
                            (fun _ ->
                                promise {
                                    Profiling.addTimestamp
                                        (fun () -> $"{nameof FsBeacon} | MountButton [ onClick ]")
                                        getLocals

                                    setMounted (not mounted)
                                })
                Children =
                    [
                        str (if mounted then "unmount" else "mount")
                    ]
            |}

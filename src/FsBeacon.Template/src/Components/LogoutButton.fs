namespace FsBeacon.Template.Components

open FsStore
open FsCore
open Fable.React
open Feliz
open FsJs
open FsStore.Hooks
open FsStore.State
open FsBeacon.Template.State
open FsUi.Bindings
open FsUi.State
open FsUi.Components


module LogoutButton =
    [<ReactComponent>]
    let LogoutButton () =
        let alias = Store.useValue Selectors.Gun.alias
        let logout = Store.useSetState Auth.Actions.logout

        let getLocals () = $"alias={alias} {getLocals ()}"
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | LogoutButton / render") getLocals

        Button.Button
            {|
                Tooltip = None
                Icon = Some (Icons.io5.IoKey |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <-
                            (fun _ ->
                                promise {
                                    Profiling.addTimestamp
                                        (fun () -> $"{nameof FsBeacon} | LogoutButton / onClick")
                                        getLocals

                                    logout ()
                                })

                        x.disabled <- alias.IsNone
                Children =
                    [
                        str $"logout ({alias})"
                    ]
            |}

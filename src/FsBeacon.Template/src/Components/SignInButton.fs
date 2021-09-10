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


module SignInButton =
    [<ReactComponent>]
    let SignInButton () =
        let alias = Store.useValue Selectors.Gun.alias
        let _ = Auth.useGunAliasLoader ()
        let getLocals () = $"alias={alias} {getLocals ()}"
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | SignInButton / render") getLocals

        let signIn = Store.useSetState Actions.signIn

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
                                        (fun () -> $"{nameof FsBeacon} | SignInButton / onClick")
                                        getLocals

                                    signIn ()
                                })

                        x.disabled <- alias.IsSome
                Children =
                    [
                        str "sign in"
                    ]
            |}

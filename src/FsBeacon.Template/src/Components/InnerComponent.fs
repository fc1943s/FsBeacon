namespace FsBeacon.Template.Components

open Browser.Types
open FsCore
open Feliz
open FsJs
open FsBeacon.Template.State
open FsUi.Bindings
open FsUi.Hooks


module InnerComponent =
    [<ReactComponent>]
    let InnerComponent () =
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | InnerComponent [ render ] ") getLocals

        React.useEffect (
            (fun () ->
                match Dom.window () with
                | Some window ->
                    window.scrollTo (
                        {|
                            left = 0.
                            top = 0.
                            behavior = ScrollBehavior.Smooth
                        |}: ScrollToOptions
                    )
                | None -> ()),
            [||]
        )

        Ui.stack
            (fun x ->
                x.id <- "component"
                x.alignItems <- "flex-start"
                x.fontSize <- "11px"
                x.maxWidth <- "100vw"
                x.flex <- "1")
            [
                Ui.stack
                    (fun _ -> ())
                    [
                        SignInButton.SignInButton ()

                        AddFileButton.AddFileButton ()

                        LogoutButton.LogoutButton ()
                    ]

                HrefIndicator.HrefIndicator ()

                Files.Files ()
            ]

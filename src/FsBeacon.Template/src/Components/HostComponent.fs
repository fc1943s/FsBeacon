namespace FsBeacon.Template.Components

open Fable.Core.JsInterop
open Fable.React
open Feliz
open FsUi.Bindings
open FsUi.Components

module HostComponent =

    [<ReactComponent>]
    let HostComponent () =
        Ui.stack
            (fun _ -> ())
            [
                Ui.box
                    (fun x ->
                        x.flex <- "1"
                        x.overflow <- "auto"
//                        x.maxWidth <- "33vw"
                        x.maxHeight <- "300px")
                    [
                        str $"#1 {Browser.Dom.window.location.href}"
                        DebugPanel.DebugPanel DebugPanel.DebugPanelDisplay.Inline
                    ]

                Ui.flex
                    (fun x -> x.flex <- "1")
                    [
                        Ui.box
                            (fun x ->
                                x.``as`` <- "iframe"
                                x.src <- "https://localhost:49212"
                                x.height <- "100vh"
                                x?``data-cy`` <- "iframe1"
                                x.flex <- "1")
                            []

                        Ui.box
                            (fun x ->
                                x.``as`` <- "iframe"
                                x.src <- "https://localhost:49222"
                                x.height <- "100vh"
                                x?``data-cy`` <- "iframe2"
                                x.flex <- "1")
                            []

                        Ui.box
                            (fun x ->
                                x.``as`` <- "iframe"
                                x.src <- "https://localhost:49222"
                                x.height <- "100vh"
                                x?``data-cy`` <- "iframe3"
                                x.flex <- "1")
                            []
                    ]
            ]

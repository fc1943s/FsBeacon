namespace FsBeacon.Template.Components

open Fable.Core.JsInterop
open Fable.React
open Feliz
open FsUi.Bindings
open FsUi.Components

module TestComponent =

    [<ReactComponent>]
    let TestComponent () =
        Ui.flex
            (fun x -> x.flex <- "1")
            [
                Ui.box
                    (fun x -> x.flex <- "1")
                    [
                        str $"#1 {Browser.Dom.window.location.href}"
                        DebugPanel.DebugPanel DebugPanel.DebugPanelDisplay.Inline
                    ]


                Ui.box
                    (fun x ->
                        x.``as`` <- "iframe"
                        x.src <- "https://localhost:49212"
                        x?``data-cy`` <- "iframe1"
                        x.flex <- "1")
                    []
                Ui.box
                    (fun x ->
                        x.``as`` <- "iframe"
                        x.src <- "https://localhost:49222"
                        x?``data-cy`` <- "iframe2"
                        x.flex <- "1")
                    []
            ]

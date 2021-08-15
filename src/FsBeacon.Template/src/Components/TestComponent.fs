namespace FsBeacon.Template.Components

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
                        DebugPanel.DebugPanel DebugPanel.DebugPanelDisplay.Inline

                        str $"ready test href={Browser.Dom.window.location.href}"
                    ]


                Ui.box
                    (fun x ->
                        x.``as`` <- "iframe"
                        x.src <- "https://localhost:49212"
                        x.flex <- "1")
                    []
                Ui.box
                    (fun x ->
                        x.``as`` <- "iframe"
                        x.src <- "https://localhost:49222"
                        x.flex <- "1")
                    []
            ]

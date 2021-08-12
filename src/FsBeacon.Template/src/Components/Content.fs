namespace FsBeacon.Template.Components

open Feliz
open FsJs
open FsStore
open FsUi.Bindings
open FsUi.Components


module Content =
    [<ReactComponent>]
    let LoggedContent () =
        Dom.log (fun () -> "LoggedContent.render.")

        React.suspense (
            [
                Ui.flex
                    (fun x -> x.flex <- "1")
                    [
                        TestComponent.TestComponent ()
                    ]
            ],
            LoadingSpinner.LoadingSpinner ()
        )

    [<ReactComponent>]
    let Content () =
        Profiling.addTimestamp "mainComponent.render"

        let deviceInfo = Store.useValue Selectors.deviceInfo

        Ui.flex
            (fun x ->
                x.flex <- "1"
                x.minHeight <- "100vh"
                x.height <- if deviceInfo.IsExtension then "590px" else null
                x.width <- if deviceInfo.IsExtension then "790px" else null)
            [
                Ui.stack
                    (fun x ->
                        x.spacing <- "0"
                        x.flex <- "1"
                        x.maxWidth <- "100vw")
                    [
                        LoggedContent ()
                    ]
            ]

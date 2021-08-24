namespace FsBeacon.Template.Components

open Feliz
open FsJs
open FsStore.Hooks
open FsStore
open FsStore.State
open FsUi.Bindings
open FsUi.Components


module Content =
    [<ReactComponent>]
    let LoggedContent () =
        Profiling.addTimestamp $"{nameof FsBeacon} | LoggedContent [ render ] "

        React.suspense (
            [
                Ui.flex
                    (fun x ->
                        x.flex <- "1"
                        x.flexDirection <- "column")
                    [
                        if Browser.Dom.window.location.port = "9762" then
                            HostComponent.HostComponent ()
                        else
                            Component.Component ()
                    ]
            ],
            LoadingSpinner.LoadingSpinner ()
        )

    [<ReactComponent>]
    let Content () =
        let deviceInfo = Store.useValue Selectors.deviceInfo

        Profiling.addTimestamp $"{nameof FsBeacon} | Content [ render ] "

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

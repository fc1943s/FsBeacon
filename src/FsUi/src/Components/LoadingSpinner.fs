namespace FsUi.Components

open FsJs
open FsUi.Bindings


module LoadingSpinner =
    let inline LoadingSpinner () =
        Profiling.addTimestamp $"{nameof FsUi} | LoadingSpinner [ render ] "

        Ui.center
            (fun x -> x.flex <- "1")
            [
                Ui.stack
                    (fun x -> x.alignItems <- "center")
                    [
                        Spinner.Spinner (fun _ -> ())
                        Ui.str "Loading..."
                    ]
            ]

    let inline InlineLoadingSpinner () =
        Profiling.addTimestamp $"{nameof FsUi} | InlineLoadingSpinner [ render ] "

        Ui.flex
            (fun x -> x.alignItems <- "center")
            [
                Spinner.Spinner
                    (fun x ->
                        x.width <- "10px"
                        x.height <- "10px")
            ]

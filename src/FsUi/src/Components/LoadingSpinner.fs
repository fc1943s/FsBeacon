namespace FsUi.Components

open FsUi.Bindings


module LoadingSpinner =
    let inline LoadingSpinner () =
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
        Ui.flex
            (fun x -> x.alignItems <- "center")
            [
                Spinner.Spinner
                    (fun x ->
                        x.width <- "10px"
                        x.height <- "10px")
            ]

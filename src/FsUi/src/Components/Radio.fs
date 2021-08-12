namespace FsUi.Components

open FsUi.Bindings


module Radio =
    let inline Radio (props: Ui.IChakraProps -> unit) children =
        Ui.stack
            (fun x ->
                x.spacing <- "4px"
                x.alignItems <- "center"
                x.direction <- "row")
            [
                Ui.radio
                    (fun x ->
                        x.colorScheme <- "purple"
                        x.borderColor <- "gray.30"
                        x.size <- "lg"
                        props x)
                    []
                Ui.box
                    (fun _ -> ())
                    [
                        yield! children
                    ]
            ]

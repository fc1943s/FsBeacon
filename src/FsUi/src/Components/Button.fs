namespace FsUi.Components

open Feliz
open Fable.React
open FsUi.Bindings


module Button =
    [<RequireQualifiedAccess>]
    type IconPosition =
        | Left
        | Right

    [<ReactComponent>]
    let Button
        (input: {| Icon: (ReactElement * IconPosition) option
                   Tooltip: ReactElement option
                   Props: Ui.IChakraProps -> unit
                   Children: seq<ReactElement> |})
        =

        let icon, iconPosition =
            match input.Icon with
            | Some (icon, iconPosition) -> Some icon, Some iconPosition
            | _ -> None, None

        Tooltip.wrap
            (input.Tooltip |> Option.defaultValue null)
            [
                match icon, input.Children |> Seq.toList with
                | Some icon, [] ->
                    Ui.iconButton
                        (fun x ->
                            x.icon <- icon
                            input.Props x)
                        []
                | icon, children ->
                    let icon () =
                        match icon with
                        | Some icon ->
                            Ui.box
                                (fun _ -> ())
                                [
                                    icon
                                ]
                        | None -> nothing

                    Ui.button
                        (fun x ->
                            x.height <- "auto"
                            x.alignSelf <- "flex-start"
                            x.color <- "gray"
                            x.paddingTop <- "5px"
                            x.paddingBottom <- "5px"
                            x.borderRadius <- "3px"
                            input.Props x)
                        [
                            Ui.stack
                                (fun x ->
                                    x.direction <- "row"
                                    x.spacing <- "7px"
                                    x.alignItems <- "center")
                                [
                                    match iconPosition with
                                    | Some IconPosition.Left -> icon ()
                                    | _ -> nothing

                                    Ui.box
                                        (fun _ -> ())
                                        [
                                            yield! children
                                        ]

                                    match iconPosition with
                                    | Some IconPosition.Right -> icon ()
                                    | _ -> nothing
                                ]
                        ]
            ]

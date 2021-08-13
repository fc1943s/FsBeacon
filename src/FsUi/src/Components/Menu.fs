namespace FsUi.Components

open Fable.Core
open Fable.React
open Feliz
open FsUi.Bindings
open FsJs
open FsCore

module Menu =
    [<ReactComponent>]
    let FakeMenuButton (cmp: (Ui.IChakraProps -> unit) -> ReactElement) (props: Ui.IChakraProps -> unit) =
        let menuContext = Ui.react.useMenuContext ()

        let menuButtonProps =
            match menuContext |> Option.ofObjUnbox with
            | Some _ -> Ui.react.useMenuButton (box {|  |})
            | None -> React.useMemo ((fun () -> Js.newObj (fun _ -> ())), [||])

        cmp
            (fun x ->
                x.``as`` <- Ui.react.Box
                x.tabIndex <- 0
                x <+ menuButtonProps

                x.onClick <-
                    fun e ->
                        menuButtonProps.onClick e |> ignore
                        e.preventDefault ()
                        JS.undefined

                x.onKeyDown <-
                    fun e ->
                        if e.key = " " then
                            menuButtonProps.onClick (unbox e) |> ignore
                        else
                            menuButtonProps.onKeyDown e |> ignore

                        if e.key = " " || e.key = "Enter" then e.preventDefault ()
                        JS.undefined

                props x)

    let inline Menu
        (input: {| Tooltip: string
                   Trigger: ReactElement
                   Body: seq<ReactElement>
                   MenuListProps: Ui.IChakraProps -> unit |})
        =
        Ui.menu
            (fun x ->
                x.isLazy <- true
                x.closeOnSelect <- false)
            [
                Tooltip.wrap
                    (str input.Tooltip)
                    [
                        input.Trigger
                    ]
                Ui.menuList
                    (fun x ->
                        x.``as`` <- Ui.react.Stack
                        x.spacing <- "2px"
                        x.backgroundColor <- "gray.13"
                        input.MenuListProps x)
                    [
                        yield! input.Body
                    ]
            ]

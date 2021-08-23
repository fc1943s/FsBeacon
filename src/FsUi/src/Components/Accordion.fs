namespace FsUi.Components

open FsJs
open FsStore
open FsStore.Hooks
open FsStore.Model
open FsUi.Bindings
open FsCore
open Feliz
open Fable.Core.JsInterop
open Fable.React


module Accordion =
    [<ReactComponent>]
    let AccordionItem title children =
        Ui.accordionItem
            (fun x ->
                if children
                   |> Seq.exists
                       (fun cmp ->
                           let props: {| props: Ui.IChakraProps |} = unbox cmp

                           match props.props.flex with
                           | String.Valid _ -> true
                           | _ -> false) then
                    x.flex <- "1"

                x.borderColor <- "gray.45"
                x.borderBottomWidth <- "0 !important"
                x.flexDirection <- "column"
                //                x.flex <- "1"
//                x.overflow <- "auto"
//                x.flexBasis <- unbox "auto"
                x.display <- "flex")
            [
                Ui.accordionButton
                    (fun x ->
                        x.backgroundColor <- "gray.16"
                        x.tabIndex <- -1)
                    [
                        Ui.box
                            (fun _ -> ())
                            [
                                title
                            ]
                        Ui.spacer (fun _ -> ()) []
                        Ui.accordionIcon (fun _ -> ()) []
                    ]

                Ui.accordionPanel
                    (fun x ->
                        x.flex <- "1"
                        x.flexDirection <- "column"
                        x.display <- "flex"
                        //                        x.overflow <- "auto"
//                        x.flexBasis <- 0
                        x.paddingTop <- "10px")
                    children
            ]

    [<ReactComponent>]
    let Accordion
        (input: {| Items: (ReactElement * ReactElement) list
                   Value: string []
                   SetValue: string [] -> unit
                   Props: Ui.IChakraProps -> unit |})
        =
        let titleArray =
            React.useMemo (
                (fun () ->
                    input.Items
                    |> List.map fst
                    |> List.toArray
                    |> Array.map
                        (fun item ->
                            if jsTypeof item = "string" then
                                string item
                            else
                                item?props
                                |> Option.ofObjUnbox
                                |> Option.map
                                    (fun props ->
                                        props?children
                                        |> Option.ofObjUnbox
                                        |> Option.map
                                            (fun children ->
                                                if jsTypeof children = "string" then
                                                    string children
                                                else
                                                    children
                                                    |> Array.tryFind (fun x -> jsTypeof x = "string")
                                                    |> Option.defaultWith (fun () -> failwith $"{item}"))
                                        |> Option.defaultWith (fun () -> failwith $"{item}"))
                                |> Option.defaultWith (fun () -> failwith $"{item}"))),
                [|
                    box input.Items
                |]
            )

        Ui.accordion
            (fun x ->
                x.allowMultiple <- true
                x.reduceMotion <- true
                x.display <- "flex"
                x.flexDirection <- "column"
                x.flex <- "1"
                x.overflow <- "auto"
                x.flexBasis <- 0
                x.defaultIndex <- [||]

                x.index <-
                    let hiddenTitleSet = input.Value |> Set.ofArray

                    titleArray
                    |> Array.indexed
                    |> Array.filter (fun (_, title) -> hiddenTitleSet.Contains title |> not)
                    |> Array.map fst
                    |> function
                        | [||] -> x.defaultIndex
                        | index -> index |> Js.toJsArray

                x.onChange <-
                    fun (indexes: obj) ->
                        promise {
                            let newIndexes =
                                match jsTypeof indexes with
                                | "number" ->
                                    match indexes |> unbox |> int with
                                    | -1 -> [||]
                                    | n ->
                                        [|
                                            n
                                        |]
                                | _ -> unbox indexes

                            let visibleTitles =
                                newIndexes
                                |> Array.map (fun index -> titleArray.[index])

                            let newHiddenTitles = titleArray |> Array.except visibleTitles

                            let newIndexes =
                                newIndexes
                                |> Array.map (fun index -> titleArray.[index])

                            if newIndexes.Length > 0 then input.SetValue newHiddenTitles
                        }

                input.Props x)
            [
                yield!
                    input.Items
                    |> List.map
                        (fun (title, cmp) ->
                            AccordionItem
                                title
                                [
                                    cmp
                                ])
            ]

    [<ReactComponent>]
    let AccordionAtom
        (input: {| Items: (ReactElement * ReactElement) list
                   Atom: AtomConfig<string []>
                   Props: Ui.IChakraProps -> unit |})
        =
        let atomValue, setAtomValue = Store.useState input.Atom

        Accordion
            {|
                Items = input.Items
                Props = input.Props
                Value = atomValue
                SetValue = setAtomValue
            |}

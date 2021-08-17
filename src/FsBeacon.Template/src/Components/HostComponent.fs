namespace FsBeacon.Template.Components

open Fable.Core.JsInterop
open Fable.React
open Feliz
open FsBeacon.Template.State
open FsUi.Bindings
open FsUi.Components

module HostComponent =

    [<ReactComponent>]
    let HostComponent () =
        Ui.stack
            (fun x -> x.flex <- "1")
            [
                Accordion.AccordionAtom
                    {|
                        Atom = Atoms.Host.accordionHiddenFlag AccordionType.Host
                        Props = fun _ -> ()
                        Items =
                            [
                                str $"Host ({Browser.Dom.window.location.href})",
                                (Ui.box
                                    (fun x ->
                                        x.overflow <- "auto"
                                        x.maxHeight <- "30vh")
                                    [
                                        DebugPanel.DebugPanel DebugPanel.DebugPanelDisplay.Inline
                                    ])

                                str "Iframes",
                                (Ui.flex
                                    (fun x -> x.flex <- "1")
                                    [
                                        Ui.flex
                                            (fun x ->
                                                x.``as`` <- "iframe"
                                                x.src <- "https://localhost:49212/#1"
                                                x?``data-cy`` <- "iframe1"
                                                x?scrolling <- "no"
                                                x.flex <- "1")
                                            []

                                        Ui.flex
                                            (fun x ->
                                                x.``as`` <- "iframe"
                                                x.src <- "https://localhost:49222/#2"
                                                x?``data-cy`` <- "iframe2"
                                                x?scrolling <- "no"
                                                x.flex <- "1")
                                            []

                                        Ui.flex
                                            (fun x ->
                                                x.``as`` <- "iframe"
                                                x.src <- "https://localhost:49222/#3"
                                                x?``data-cy`` <- "iframe3"
                                                x?scrolling <- "no"
                                                x.flex <- "1")
                                            []
                                    ])
                            ]
                    |}
            ]

namespace FsBeacon.Template.Components

open FsCore
open Fable.Core.JsInterop
open Fable.React
open Feliz
open FsBeacon.Template.State
open FsBeacon.Template.State.Host
open FsJs
open FsUi.Bindings
open FsUi.Components

module HostComponent =

    [<ReactComponent>]
    let HostComponent () =
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | HostComponent [ render ] ") getLocals

        Ui.stack
            (fun x -> x.flex <- "1")
            [
                Accordion.AccordionAtom
                    {|
                        Atom = Atoms.Host.accordionHiddenFlag AccordionType.HostComponent
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
                                                x.src <- "https://localhost:49222/#1"
                                                x?``data-cy`` <- "iframe1"
                                                x.flex <- "1")
                                            []

                                        Ui.flex
                                            (fun x ->
                                                x.``as`` <- "iframe"
                                                x.src <- "https://localhost:49222/#2"
                                                x?``data-cy`` <- "iframe2"
                                                x.flex <- "1")
                                            []

                                        Ui.flex
                                            (fun x ->
                                                x.``as`` <- "iframe"
                                                x.src <- "https://localhost:49222/#3"
                                                x?``data-cy`` <- "iframe3"
                                                x.flex <- "1")
                                            []
                                    ])
                            ]
                    |}
            ]

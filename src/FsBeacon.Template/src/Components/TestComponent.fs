namespace FsBeacon.Template.Components

open Fable.React
open Feliz
open FsUi.Bindings

module TestComponent =

    [<ReactComponent>]
    let TestComponent () =

        Ui.box
            (fun _ -> ())
            [
                str "ready"
            ]

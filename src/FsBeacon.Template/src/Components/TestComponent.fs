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
                Html.iframe [
                    prop.src "https://localhost:49212"
                    prop.width 400
                    prop.height 400
                ]
                Html.iframe [
                    prop.src "https://localhost:49222"
                    prop.width 400
                    prop.height 400
                ]
                str "ready"
            ]

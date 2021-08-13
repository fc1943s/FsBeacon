namespace FsBeacon.Template.Components


open Fable.React
open Feliz
open FsUi.Bindings

module Component =

    [<ReactComponent>]
    let Component () =
        Ui.box
            (fun _ -> ())
            [
                str $"ready component href={Browser.Dom.window.location.href}"
            ]

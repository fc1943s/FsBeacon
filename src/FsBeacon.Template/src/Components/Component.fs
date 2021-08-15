namespace FsBeacon.Template.Components


open Fable.React
open Feliz
open FsUi.Bindings
open FsUi.Components

module Component =

    [<ReactComponent>]
    let Component () =
        Ui.box
            (fun _ -> ())
            [
                DebugPanel.DebugPanel DebugPanel.DebugPanelDisplay.Inline

                str $"ready component href={Browser.Dom.window.location.href}"
            ]

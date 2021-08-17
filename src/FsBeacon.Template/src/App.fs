namespace FsBeacon.Template

open Feliz
open FsJs
open FsUi.Components
open FsBeacon.Template.Components


module App =

    [<ReactComponent>]
    let App wrap =
        Profiling.addTimestamp "App().render"
        Profiling.addCount "App().render"

        (if wrap then RootWrapper.RootWrapper None else React.fragment)
            [
                Content.Content ()
            ]

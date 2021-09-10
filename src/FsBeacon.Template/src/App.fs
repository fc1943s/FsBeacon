namespace FsBeacon.Template

open FsCore
open Feliz
open FsJs
open FsUi.Components
open FsBeacon.Template.Components


module App =

    [<ReactComponent>]
    let App wrap =
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | App [ render ] ") getLocals

        (if wrap then RootWrapper.RootWrapper None else React.fragment)
            [
                Content.Content ()
            ]

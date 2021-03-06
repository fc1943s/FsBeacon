namespace FsBeacon.Template

open FsCore
open Fable.Core
open FsUi.Bindings
open Feliz
open Fable.Core.JsInterop
open FsJs


module Main =
    exportDefault (
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | Main body") getLocals

        let cmp = React.strictMode [ App.App true ]

        match Dom.window () with
        | Some window ->
            React.render (window.document.getElementById "root") cmp
            JS.undefined
        | None -> cmp
    )

namespace FsBeacon.Template.Components

open FsStore
open FsCore
open Fable.React
open Feliz
open FsJs
open FsStore.Hooks
open FsStore.State
open FsBeacon.Template.State
open FsUi.State


module HrefIndicator =
    [<ReactComponent>]
    let HrefIndicator () =
        let _routeTrigger = Store.useValue Atoms.routeTrigger

        let inline getLocals () =
            $"_routeTrigger={_routeTrigger} {getLocals ()}"

        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | HrefIndicator [ render ]") getLocals

        str $"href: {Browser.Dom.window.location.href}"


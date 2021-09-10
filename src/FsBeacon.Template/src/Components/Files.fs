namespace FsBeacon.Template.Components

open FsStore
open FsCore
open Browser.Types
open Fable.React
open Feliz
open FsJs
open FsStore.Bindings.Gun
open FsStore.Hooks
open FsStore.Model
open FsStore.State
open FsBeacon.Template.State
open FsBeacon.Template
open FsUi.Bindings
open FsUi.Hooks
open FsUi.State
open FsUi.Components


module Files =
    [<ReactComponent>]
    let Files () =
        let fileIdAtoms = Store.useValue Selectors.Sample.fileIdAtoms

        Profiling.addTimestamp
            (fun () -> $"{nameof FsBeacon} | Files [ render ] fileIdAtoms.Length={fileIdAtoms.Length}") getLocals
        //        let fileIdAtoms = Store.useValue State.Selectors.asyncFileIdAtoms

        React.fragment [
            br []
            str $"file count: {fileIdAtoms.Length}"
            yield! fileIdAtoms |> Array.mapi File.File
        ]


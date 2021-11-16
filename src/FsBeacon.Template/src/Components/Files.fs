namespace FsBeacon.Template.Components

open FsStore
open FsCore
open Fable.React
open Feliz
open FsJs
open FsStore.Hooks
open FsStore.State
open FsBeacon.Template.State
open FsUi.Bindings
open FsUi.Hooks
open FsUi.State


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


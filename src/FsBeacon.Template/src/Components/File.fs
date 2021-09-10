namespace FsBeacon.Template.Components

open FsStore
open FsCore
open Fable.React
open Feliz
open FsJs
open FsStore.Hooks
open FsStore.Model
open FsStore.State
open FsBeacon.Template.State
open FsUi.Bindings
open FsUi.State
open FsUi.Components


module File =
    let dataChar = "#"
    let dataBlob = Fable.SimpleHttp.Blob.fromText (String.init (Hydrate.fileChunkSize * 1) (fun _ -> dataChar))
    let hexStringPromise = Js.blobToHexString dataBlob


    [<ReactComponent>]
    let File index fileIdAtom =
        let fileId = Store.useValue fileIdAtom
        let progress = Store.useValue (Selectors.File.progress fileId)

        let getLocals () =
            $"fileId={fileId} index={index + 1} progress={progress} {getLocals ()}"

        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | File [ render ]") getLocals

        let deleteFile =
            Store.useCallbackRef
                (fun getter _setter _ ->
                    promise {
                        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | File [ deleteFile ] ") getLocals

                        let storeAtomPath =
                            RecordAtomPath (FsStore.storeRoot, Atoms.File.collection, Atoms.File.formatFileId fileId)

                        do! Engine.delete getter storeAtomPath
                    })

        Ui.flex
            (fun x -> x.whiteSpace <- "nowrap")
            [
                str $"fileId={fileId} index={index + 1} progress={progress}%%"

                Button.Button
                    {|
                        Icon = Some (Icons.bi.BiTrash |> Icons.render, Button.IconPosition.Left)
                        Tooltip = None
                        Props = fun x -> x.onClick <- (fun _ -> deleteFile ())
                        Children =
                            [
                                str $"[{index + 1}]:delete"
                            ]
                    |}
            ]

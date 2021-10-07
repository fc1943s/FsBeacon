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
open FsUi.State
open FsUi.Components


module AddFileButton =
    let dataChar = "#"
    let dataBlob = Fable.SimpleHttp.Blob.fromText (String.init (Hydrate.fileChunkSize * 2) (fun _ -> dataChar))
    let hexStringPromise = Js.blobToHexString dataBlob


    [<ReactComponent>]
    let AddFileButton () =
        let alias = Store.useValue Selectors.Gun.alias

        let inline getLocals () = $"alias={alias} {getLocals ()}"
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | AddFileButton [ render ]") getLocals

        let addFile =
            Store.useCallbackRef
                (fun _ setter _ ->
                    promise {
                        Profiling.addTimestamp
                            (fun () -> $"{nameof FsBeacon} | AddFileButton [ render ] addFile()")
                            getLocals

                        let! hexString = hexStringPromise
                        let fileId = Hydrate.hydrateFile setter hexString

                        let getLocals () = $"fileId={fileId} {getLocals ()}"
                        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | addFile callback completed") getLocals
                    })

        Button.Button
            {|
                Tooltip = None
                Icon = Some (Icons.io5.IoAdd |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <-
                            (fun _ ->
                                Profiling.addTimestamp
                                    (fun () -> $"{nameof FsBeacon} | AddFileButton [ onClick ]")
                                    getLocals

                                addFile ())

                        x.disabled <- alias.IsNone
                Children =
                    [
                        str "add file"
                    ]
            |}

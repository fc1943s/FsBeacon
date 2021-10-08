namespace FsBeacon.HubPeer

open FsCore
open FsClr
open System.IO

module Sync =
    [<RequireQualifiedAccess>]
    type Request =
        | Connect of alias: string
        | Set of alias: string * atomPath: string * value: string
        | Get of alias: string * atomPath: string
        | Keys of alias: string * collectionPath: string

    [<RequireQualifiedAccess>]
    type Response =
        | ConnectResult
        | SetResult of ok: bool
        | GetResult of value: string option
        | GetStream of alias: string * atomPath: string * value: string option
        | KeysResult of keys: string []
        | KeysStream of alias: string * collectionPath: string * updatedKeys: string [] * removedKeys: string []


module FileSystem =
    let rec getStreamAsync path =
        async {
            try
                return
                    if File.Exists path then
                        Some (new FileStream (path, FileMode.Open, FileAccess.Write))
                    else
                        None
            with
            | _ ->
                let getLocals () = $"path={path} {getLocals ()}"

                Logger.logWarning
                    (fun () ->
                        $"{nameof FsBeacon} | FileSystem.getStreamAsync. Error opening file for writing. Waiting...")
                    getLocals

                do! Async.Sleep 100
                return! getStreamAsync path
        }

    let waitForFileWriteAsync path =
        getStreamAsync path
        |> Async.map (Option.iter (fun stream -> stream.Close ()))


module String =
    let inline trim (str: string) = str.Trim ()

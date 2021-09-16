namespace FsBeacon.HubPeer

open FsCore
open FsClr
open System.IO

module FileSystem =
    let rec getStreamAsync path =
        async {
            try
                if File.Exists path then
                    return Some (new FileStream (path, FileMode.Open, FileAccess.Write))
                else
                    return None
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

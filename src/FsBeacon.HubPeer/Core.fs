namespace FsBeacon.HubPeer

open FsCore
open FsClr
open System.IO

module FileSystem =
    let rec getStreamAsync path =
        async {
            try
                return new FileStream (path, FileMode.Open, FileAccess.Write)
            with
            | _ ->
                let getLocals () = $"path={path} {getLocals ()}"

                Logger.logWarning
                    (fun () -> "FileSystem.getStreamAsync. Error opening file for writing. Waiting...")
                    getLocals

                do! Async.Sleep 100
                return! getStreamAsync path
        }

    let waitForFileWriteAsync path =
        async {
            do!
                getStreamAsync path
                |> Async.map (fun stream -> stream.Close ())
        }

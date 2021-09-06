namespace FsBeacon.HubPeer

open System
open System.IO
open System.Reflection
open Argu
open FSharp.Control
open Serilog


module Startup =
    let inline parseArgs<'T when 'T :> IArgParserTemplate> args =
        let errorHandler =
            ProcessExiter (
                colorizer =
                    function
                    | ErrorCode.HelpText -> None
                    | _ -> Some ConsoleColor.Red
            )

        let parser =
            ArgumentParser.Create<'T> (
                programName =
                    Assembly.GetEntryAssembly().GetName().Name
                    + ".exe",
                errorHandler = errorHandler
            )

        parser.ParseCommandLine args


module Logger =
    let inline logDebug fn getLocals =
        if true then Log.Debug $"{fn ()} {getLocals ()}"

    let inline logTrace fn getLocals =
        if true then Log.Verbose $"{fn ()} {getLocals ()}"


module FileSystem =
    type FileSystemChange =
        | Changed of FileSystemEventArgs
        | Created of FileSystemEventArgs
        | Deleted of FileSystemEventArgs
        | Renamed of RenamedEventArgs

    let inline watch path =
        let getLocals () = $"path={path}"
        Logger.logDebug (fun () -> "FileSystem.watchFileSystem") getLocals

        use watcher = new FileSystemWatcher (Path = path, EnableRaisingEvents = true, IncludeSubdirectories = true)

        [
            AsyncSeq.forwardEvent FileSystemChange.Changed watcher.Changed.Add
            AsyncSeq.forwardEvent FileSystemChange.Created watcher.Created.Add
            AsyncSeq.forwardEvent FileSystemChange.Deleted watcher.Deleted.Add
            AsyncSeq.forwardEvent FileSystemChange.Renamed watcher.Renamed.Add
        ]
        |> AsyncSeq.mergeAll

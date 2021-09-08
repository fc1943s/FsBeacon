namespace FsBeacon.HubPeer

open System
open System.IO
open System.Reflection
open Argu
open FSharp.Control
open Serilog
open FsCore


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
        | Error of exn: exn
        | Changed of path: string
        | Created of path: string
        | Deleted of path: string
        | Renamed of oldPath: string * path: string

    let inline watch path =
        let watcher = new FileSystemWatcher (Path = path, EnableRaisingEvents = true, IncludeSubdirectories = true)

        let changedStream =
            AsyncSeq.subscribeEvent watcher.Changed (fun event -> FileSystemChange.Changed event.FullPath)
        //            |> AsyncSeq.bufferByTime 100
//            |> AsyncSeq.choose Array.tryLast

        let createdStream =
            AsyncSeq.subscribeEvent watcher.Created (fun event -> FileSystemChange.Created event.FullPath)

        let deletedStream =
            AsyncSeq.subscribeEvent watcher.Deleted (fun event -> FileSystemChange.Deleted event.FullPath)

        let renamedStream =
            AsyncSeq.subscribeEvent
                watcher.Renamed
                (fun event -> FileSystemChange.Renamed (event.OldFullPath, event.FullPath))

        let errorStream =
            AsyncSeq.subscribeEvent watcher.Error (fun event -> FileSystemChange.Error (event.GetException ()))

        let stream =
            [
                changedStream
                createdStream
                deletedStream
                renamedStream
                errorStream
            ]
            |> AsyncSeq.mergeAll

        let disposable =
            Object.newDisposable
                (fun () ->
                    watcher.EnableRaisingEvents <- false
                    watcher.Dispose ())

        stream, disposable

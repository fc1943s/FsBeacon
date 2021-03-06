namespace FsBeacon.HubPeer

open FsClr
open FsClr.FileSystem
open FsCore
open System.Collections.Concurrent
open System.IO
open System.Threading.Tasks
open FSharp.Control
open Fable.SignalR
open FsBeacon.Shared
open FSharp.Control.Tasks.V2
open System


module HubServer =
    let createParentDirectory path =
        Directory.CreateDirectory (Directory.GetParent path).FullName
        |> ignore

    type AtomRef = AtomRef of alias: string * atomPath: string

    let writeFile rootPath (AtomRef (alias, atomPath)) value =
        task {
            try
                let path = Path.Combine (rootPath, alias, atomPath)

                match Guid.TryParse (Path.GetFileName path) with
                | true, _ when isNull value -> Directory.Delete (path, true)
                | _ ->
                    createParentDirectory path
                    do! File.WriteAllTextAsync (path, value)

                return true
            with
            | ex ->
                eprintfn $"writeFile error ex={ex.Message}"
                return false
        }

    let readFile rootPath (AtomRef (alias, atomPath)) =
        task {
            try
                let path = Path.Combine (rootPath, alias, atomPath)

                if File.Exists path then
                    do! FileSystem.waitForFileWriteAsync path
                    let! result = File.ReadAllTextAsync path
                    return result |> Option.ofObj
                else
                    return None
            with
            | _ex ->
                eprintfn $"readFile error ex={_ex.Message}"
                return None
        }

    //    let keyWatchlist = ConcurrentDictionary<AtomRef, TicksGuid * string [] option> ()

    let fetchTableKeys rootPath (AtomRef (alias, atomPath)) =
        let path = Path.Combine (rootPath, alias, atomPath)
        Directory.CreateDirectory path |> ignore

        Directory.EnumerateDirectories path
        |> Seq.map Path.GetFileName
        |> Seq.toArray

    //    let trySubscribeKeys atomRef =
//        if keyWatchlist.ContainsKey atomRef |> not then
//            keyWatchlist.[atomRef] <- (Guid.newTicksGuid (), Some [||])

    let substringTo n (str: string) =
        match str with
        | String.Valid _ when str.Length > n -> str |> String.substring 0 n
        | _ -> str

    let update rootPath (msg: Sync.Request) (_hubContext: FableHub<Sync.Request, Sync.Response> option) =
        task {
            match msg with
            | Sync.Request.Connect alias ->
                let getLocals () = $"alias={alias} {getLocals ()}"
                Logger.logDebug (fun () -> $"{nameof FsBeacon} | Hub.update (Sync.Request.Connect)") getLocals
                return Sync.Response.ConnectResult
            | Sync.Request.Set (alias, atomPath, value) ->
                let atomRef = AtomRef (alias, atomPath)
                let! result = writeFile rootPath atomRef value

                let getLocals () =
                    $"alias={alias} atomPath={atomPath} value={value |> substringTo 40} result={result}  {getLocals ()}"

                Logger.logDebug (fun () -> $"{nameof FsBeacon} | Hub.update (Sync.Request.Set)") getLocals
                return Sync.Response.SetResult result
            | Sync.Request.Get (alias, atomPath) ->
                let atomRef = AtomRef (alias, atomPath)
                let! value = readFile rootPath atomRef

                let getLocals () =
                    $"alias={alias} atomPath={atomPath} value={value |> Option.map (substringTo 40)} {getLocals ()}"

                Logger.logDebug (fun () -> $"{nameof FsBeacon} | Hub.update (Sync.Request.Get)") getLocals
                return Sync.Response.GetResult value
            | Sync.Request.Keys (alias, collectionPath) ->
                let collectionRef = AtomRef (alias, collectionPath)
                //                trySubscribeKeys collectionRef
                let result = fetchTableKeys rootPath collectionRef

                let getLocals () =
                    $"alias={alias} collectionPath={collectionPath} result=%A{result} {getLocals ()}"

                Logger.logDebug (fun () -> $"{nameof FsBeacon} | Hub.update (Sync.Request.Filter)") getLocals
                return Sync.Response.KeysResult result
        }

    let invoke rootPath (msg: Sync.Request) _ = update rootPath msg None

    let send rootPath (msg: Sync.Request) (hubContext: FableHub<Sync.Request, Sync.Response>) =
        task {
            let! response = update rootPath msg (Some hubContext)
            do! hubContext.Clients.Caller.Send response
        }

    let tryCleanup (dict: ConcurrentDictionary<AtomRef, TicksGuid * _>) =
        dict
        |> Seq.iter
            (fun (KeyValue (atomRef, (lastTicks, lastValue))) ->
                let ticksDiff =
                    lastTicks
                    |> Guid.ticksFromGuid
                    |> DateTime.ticksDiff

                let minutes = 5

                if ticksDiff > (TimeSpan.FromSeconds 60. * float minutes)
                    .TotalMilliseconds then
                    let getLocals () =
                        $"minutes={minutes} ticksDiff{ticksDiff} lastValue={lastValue} atomRef={atomRef} {getLocals ()}"

                    Logger.logDebug
                        (fun () ->
                            $"{nameof FsBeacon} | Hub.tryCleanup / dict.iter() (ticksDiff. removing from watchlist)")
                        getLocals

                    dict.TryRemove atomRef |> ignore)

    //    let tick rootPath sendAll =
//        task {
//            let getLocals () =
//                $"keyWatchlist.Count={keyWatchlist.Count} {getLocals ()}"
//
//            tryCleanup keyWatchlist
//
//            do!
//                keyWatchlist
//                |> Seq.choose
//                    (fun (KeyValue (AtomRef (alias, atomPath) as atomRef, lastKeys)) ->
//                        let result = fetchTableKeys rootPath atomRef
//
//                        let getLocals () =
//                            $"alias={alias} atomPath={atomPath} lastKeys=%A{lastKeys} result=%A{result} {getLocals ()}"
//
//
//                        match lastKeys, result with
//                        | (_, None), _ -> None
//                        | (_, Some lastKeys), result when lastKeys = result -> None
//                        | (_, Some _), result ->
//                            Logger.logTrace (fun () -> $"{nameof FsBeacon} | Hub.tick / keyWatchlist.choose") getLocals
//                            keyWatchlist.[atomRef] <- (Guid.newTicksGuid (), Some result)
//                            Some (alias, atomPath, result)
//                        | _ -> None)
//                |> Seq.toArray
//                |> Seq.map (Sync.Response.FilterStream >> sendAll)
//                |> Task.WhenAll
//        }

    let fileEvent rootPath (sendAll: Sync.Response -> Task) ticks event =
        let oldPath, newPath = event |> FileSystemChange.Path

        let getLocals () =
            $"ticks={ticks} oldPath={oldPath} newPath={newPath} {getLocals ()}"

        match newPath with
        | Some newPath when
            Directory.Exists newPath |> not
            && newPath.EndsWith ".tmp" |> not
            ->
            let fullAtomPath = newPath |> String.replace rootPath ""

            let nodes =
                fullAtomPath
                |> String.split (string Path.DirectorySeparatorChar)
                |> Array.toList

            match nodes with
            | _ :: alias :: storeRoot :: tail ->
                let collection, keys, name =
                    match tail with
                    | [ name ] -> None, [], name
                    | collection :: [ name ] -> Some collection, [], name
                    | collection :: tail ->
                        let name = tail |> List.last
                        let keys = tail |> List.take (tail.Length - 1)
                        Some collection, keys, name
                    | _ -> failwith $"invalid newPath {getLocals ()}"

                let collectionPath =
                    [
                        yield storeRoot
                        match collection with
                        | Some collection -> yield collection
                        | _ -> ()
                    ]
                    |> String.concat "/"

                let atomPath =
                    [
                        yield collectionPath
                        yield! keys
                        yield name
                    ]
                    |> String.concat "/"

                let getLocals () =
                    $"alias={alias} atomPath={atomPath} keys={keys} {getLocals ()}"


                //                match collection with
//                | Some collection ->
//                    let collectionRef = AtomRef (alias, $"{storeRoot}/{collection}")
//                    trySubscribeKeys collectionRef
//                | _ -> ()

                async {
                    let! value =
                        readFile rootPath (AtomRef (alias, atomPath))
                        |> Async.AwaitTask

                    let getLocals () =
                        $"value={string value |> substringTo 40} {getLocals ()}"

                    match value with
                    | Some value ->
                        try
                            let response = Sync.Response.GetStream (alias, atomPath, Some value)
                            do! sendAll response |> Async.AwaitTask

                            Logger.logTrace
                                (fun () -> $"{nameof FsBeacon} | HubServer.fileEvent / GetStream sent")
                                getLocals

                            match keys with
                            | [ key ] ->
                                let response =
                                    Sync.Response.KeysStream (alias, collectionPath, Array.singleton key, [||])

                                do! sendAll response |> Async.AwaitTask

                                Logger.logTrace
                                    (fun () -> $"{nameof FsBeacon} | HubServer.fileEvent / KeysStream sent (update)")
                                    getLocals
                            | _ ->
                                Logger.logTrace
                                    (fun () -> $"{nameof FsBeacon} | HubServer.fileEvent / KeysStream update skipped")
                                    getLocals
                        with
                        | ex ->
                            let getLocals () = $"ex={ex} {getLocals ()}"

                            Logger.logError
                                (fun () -> $"{nameof FsBeacon} | HubServer.fileEvent / sendAll error")
                                getLocals

                    | None ->
                        match keys with
                        | [ key ] when
                            event |> FileSystemChange.Type = FileSystemChangeType.Deleted
                            && Directory.Exists collectionPath |> not
                            ->
                            let response = Sync.Response.KeysStream (alias, collectionPath, [||], Array.singleton key)
                            do! sendAll response |> Async.AwaitTask

                            Logger.logTrace
                                (fun () -> $"{nameof FsBeacon} | HubServer.fileEvent / KeysStream sent (delete)")
                                getLocals
                        | _ ->
                            Logger.logTrace
                                (fun () -> $"{nameof FsBeacon} | HubServer.fileEvent / GetStream skipped")
                                getLocals
                }
                |> Async.RunSynchronously
            | _ -> Logger.logTrace (fun () -> $"{nameof FsBeacon} | HubServer.fileEvent / skipped 1") getLocals
        | _ -> Logger.logTrace (fun () -> $"{nameof FsBeacon} | HubServer.fileEvent / skipped 2") getLocals

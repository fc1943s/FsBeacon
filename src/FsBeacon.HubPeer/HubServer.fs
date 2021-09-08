namespace FsBeacon.HubPeer

open FsClr
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
    let inline createParentDirectory path =
        Directory.CreateDirectory (Directory.GetParent(path).FullName)
        |> ignore

    type AtomRef = AtomRef of alias: string * atomPath: string

    let inline writeFile rootPath (AtomRef (alias, atomPath)) value =
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

    let inline readFile rootPath (AtomRef (alias, atomPath)) =
        task {
            try
                let path = Path.Combine (rootPath, alias, atomPath)

                if File.Exists path then
                    let! result = File.ReadAllTextAsync path
                    return result |> Option.ofObj
                else
                    return None
            with
            | _ex ->
                eprintfn $"readFile error ex={_ex.Message}"
                return None
        }

    let keyWatchlist = ConcurrentDictionary<AtomRef, TicksGuid * string [] option> ()

    let inline fetchTableKeys rootPath (AtomRef (alias, atomPath)) =
        let path = Path.Combine (rootPath, alias, atomPath)
        Directory.CreateDirectory path |> ignore

        Directory.EnumerateDirectories path
        |> Seq.map Path.GetFileName
        |> Seq.toArray

    let inline trySubscribeKeys atomRef =
        if keyWatchlist.ContainsKey atomRef |> not then
            keyWatchlist.[atomRef] <- (Guid.newTicksGuid (), Some [||])

    let inline substringTo n (str: string) =
        if str.Length > n then str |> String.substring 0 n else str

    let inline update rootPath (msg: Sync.Request) (_hubContext: FableHub<Sync.Request, Sync.Response> option) =
        task {
            let getLocals () =
                $"rootPath={rootPath} msg={string msg |> substringTo 400} {getLocals ()}"

            match msg with
            | Sync.Request.Connect _alias ->
                Logger.logDebug (fun () -> "Hub.update (Sync.Request.Connect)") getLocals
                return Sync.Response.ConnectResult
            | Sync.Request.Set (alias, atomPath, value) ->
                let atomRef = AtomRef (alias, atomPath)
                let! result = writeFile rootPath atomRef value

                let getLocals () =
                    $"result={result} value={value |> substringTo 400} {getLocals ()}"

                Logger.logDebug (fun () -> "Hub.update (Sync.Request.Set)") getLocals
                return Sync.Response.SetResult result
            | Sync.Request.Get (alias, atomPath) ->
                let atomRef = AtomRef (alias, atomPath)
                let! value = readFile rootPath atomRef

                let getLocals () =
                    $"value={value |> Option.map (substringTo 400)} {getLocals ()}"

                Logger.logDebug (fun () -> "Hub.update (Sync.Request.Get)") getLocals
                return Sync.Response.GetResult value
            | Sync.Request.Filter (alias, atomPath) ->
                let collectionRef = AtomRef (alias, atomPath)
                trySubscribeKeys collectionRef
                let result = fetchTableKeys rootPath collectionRef
                let getLocals () = $"result=%A{result} {getLocals ()}"
                Logger.logDebug (fun () -> "Hub.update (Sync.Request.Filter)") getLocals
                return Sync.Response.FilterResult result
        }

    let inline invoke rootPath (msg: Sync.Request) _ = update rootPath msg None

    let inline send rootPath (msg: Sync.Request) (hubContext: FableHub<Sync.Request, Sync.Response>) =
        task {
            let! response = update rootPath msg (Some hubContext)
            do! hubContext.Clients.Caller.Send response
        }

    let inline tryCleanup (dict: ConcurrentDictionary<AtomRef, TicksGuid * _>) =
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
                        (fun () -> "Hub.tryCleanup / dict.iter() (ticksDiff. removing from watchlist)")
                        getLocals

                    dict.TryRemove atomRef |> ignore)

    let inline tick rootPath sendAll =
        task {
            let getLocals () =
                $"rootPath={rootPath} keyWatchlist.Count={keyWatchlist.Count} {getLocals ()}"

            tryCleanup keyWatchlist

            do!
                keyWatchlist
                |> Seq.choose
                    (fun (KeyValue (AtomRef (alias, atomPath) as atomRef, lastKeys)) ->
                        let result = fetchTableKeys rootPath atomRef

                        let getLocals () =
                            $"alias={alias} atomPath={atomPath} lastKeys=%A{lastKeys} result=%A{result} {getLocals ()}"


                        match lastKeys, result with
                        | (_, None), _ -> None
                        | (_, Some lastKeys), result when lastKeys = result -> None
                        | (_, Some _), result ->
                            Logger.logTrace (fun () -> "Hub.tick / keyWatchlist.choose") getLocals
                            keyWatchlist.[atomRef] <- (Guid.newTicksGuid (), Some result)
                            Some (alias, atomPath, result)
                        | _ -> None)
                |> Seq.toArray
                |> Seq.map (Sync.Response.FilterStream >> sendAll)
                |> Task.WhenAll
        }

    let inline fileEvent rootPath _sendAll event =
        let getLocals () =
            $"rootPath={rootPath} event={event} {getLocals ()}"

        Logger.logTrace (fun () -> "Hub.fileEvent") getLocals

//            sendAll (Sync.Response.GetStream (alias))
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
//                            Logger.logTrace (fun () -> "Hub.tick / keyWatchlist.choose") getLocals
//                            keyWatchlist.[atomRef] <- (Guid.newTicksGuid (), Some result)
//                            Some (alias, atomPath, result)
//                        | _ -> None)
//                |> Seq.toArray
//                |> Seq.map (Sync.Response.GetStream >> sendAll)
//                |> Task.WhenAll

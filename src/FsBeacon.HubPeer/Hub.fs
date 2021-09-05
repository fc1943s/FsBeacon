namespace FsBeacon.HubPeer

open FsCore
open System.Collections.Concurrent
open System.IO
open System.Threading.Tasks
open FSharp.Control
open System.Threading
open Fable.SignalR
open FsBeacon.Shared
open FSharp.Control.Tasks.V2
open System
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Serilog


module Hub =
    let inline createParentDirectory path =
        Directory.CreateDirectory (Directory.GetParent(path).FullName)
        |> ignore

    type AtomRef = AtomRef of alias: string * atomPath: string

    let inline writeFile rootPath (AtomRef (alias, atomPath)) value =
        task {
            try
                let path = Path.Combine (rootPath, alias, atomPath)

                match Guid.TryParse (Path.GetFileName path) with
                | true, _ when value = null -> Directory.Delete (path, true)
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
                let! result = File.ReadAllTextAsync path
                return result |> Option.ofObj
            with
            | _ex ->
                eprintfn $"readFile error ex={_ex.Message}"
                return None
        }

    let keyWatchlist = ConcurrentDictionary<AtomRef, TicksGuid * string [] option> ()
    let atomWatchlist = ConcurrentDictionary<AtomRef, TicksGuid * string option> ()

    let inline fetchTableKeys rootPath (AtomRef (alias, atomPath)) =
        let path = Path.Combine (rootPath, alias, atomPath)
        Directory.CreateDirectory path |> ignore

        Directory.EnumerateDirectories path
        |> Seq.map Path.GetFileName
        |> Seq.toArray

    let inline trySubscribeKeys atomRef =
        if keyWatchlist.ContainsKey atomRef |> not then
            keyWatchlist.[atomRef] <- (Guid.newTicksGuid (), Some [||])

    let inline addDebug fn getLocals = Log.Debug $"{fn ()} {getLocals ()}"
    let inline addTrace fn getLocals = Log.Verbose $"{fn ()} {getLocals ()}"

    let inline trySubscribeAtom (AtomRef (alias, atomPath) as atomRef) =
        if atomWatchlist.ContainsKey atomRef |> not then
            atomWatchlist.[atomRef] <- (Guid.newTicksGuid (), Some "")

        match atomPath |> String.split "/" |> Array.toList with
        | storeRoot :: collection :: Guid.Valid _ :: _ ->
            let collectionRef = AtomRef (alias, $"{storeRoot}/{collection}")
            trySubscribeKeys collectionRef

            let getLocals () =
                $"collectionRef={collectionRef} {getLocals ()}"

            addDebug (fun () -> "[ trySubscribeAtom ] key subscribe") getLocals
        | _ -> ()

    let inline substringTo n (str: string) =
        if str.Length > n then str |> String.substring 0 n else str

    let inline update rootPath (msg: Sync.Request) (_hubContext: FableHub<Sync.Request, Sync.Response> option) =
        task {
            let getLocals () =
                $"rootPath={rootPath} msg={string msg |> substringTo 400} {getLocals ()}"

            let inline addDebug fn getLocals =
                addDebug (fun () -> $"Hub.update {fn ()}") getLocals

            match msg with
            | Sync.Request.Connect _alias ->
                addDebug (fun () -> "[ Sync.Request.Connect ]") getLocals
                return Sync.Response.ConnectResult
            | Sync.Request.Set (alias, atomPath, value) ->
                let atomRef = AtomRef (alias, atomPath)
                let! result = writeFile rootPath atomRef value
                addDebug (fun () -> "[ Sync.Request.Set ]") getLocals
                trySubscribeAtom atomRef
                atomWatchlist.[atomRef] <- (Guid.newTicksGuid (), Some value)
                return Sync.Response.SetResult result
            | Sync.Request.Get (alias, atomPath) ->
                let atomRef = AtomRef (alias, atomPath)
                let! value = readFile rootPath atomRef
                let getLocals () = $"value={value} {getLocals ()}"
                addDebug (fun () -> "[ Sync.Request.Get ]") getLocals
                return Sync.Response.GetResult value
            | Sync.Request.Filter (alias, atomPath) ->
                let collectionRef = AtomRef (alias, atomPath)
                trySubscribeKeys collectionRef
                let result = fetchTableKeys rootPath collectionRef
                let getLocals () = $"result=%A{result} {getLocals ()}"
                addDebug (fun () -> "[ Sync.Request.Filter ]") getLocals
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

                if ticksDiff > (TimeSpan.FromMinutes 5.).TotalMilliseconds then
                    let getLocals () =
                        $"ticksDiff{ticksDiff} lastValue={lastValue} atomRef={atomRef} {getLocals ()}"

                    addTrace (fun () -> "- removing from watchlist") getLocals

                    keyWatchlist.TryRemove atomRef |> ignore)

    let inline tick rootPath sendAll =
        task {
            let getLocals () =
                $"rootPath={rootPath} keyWatchlist.Count={keyWatchlist.Count} atomWatchlist.Count={atomWatchlist.Count} {getLocals ()}"

            let inline _addDebug fn getLocals =
                addDebug (fun () -> $"Hub.tick {fn ()}") getLocals

            let inline addTrace fn getLocals =
                addTrace (fun () -> $"Hub.tick {fn ()}") getLocals

            tryCleanup keyWatchlist
            tryCleanup atomWatchlist

            do!
                keyWatchlist
                |> Seq.choose
                    (fun (KeyValue (AtomRef (alias, atomPath) as atomRef, lastKeys)) ->
                        let result = fetchTableKeys rootPath atomRef

                        let getLocals () =
                            $"alias={alias} atomPath={atomPath} lastKeys=%A{lastKeys} result=%A{result} {getLocals ()}"

                        addTrace (fun () -> "[ keyWatchlist.choose ]") getLocals

                        match lastKeys, result with
                        | (_, None), _ -> None
                        | (_, Some lastKeys), result when lastKeys = result -> None
                        | (_, Some _), result ->
                            keyWatchlist.[atomRef] <- (Guid.newTicksGuid (), Some result)
                            Some (alias, atomPath, result)
                        | _ -> None)
                |> Seq.toArray
                |> Seq.map (Sync.Response.FilterStream >> sendAll)
                |> Task.WhenAll

            do!
                atomWatchlist
                |> Seq.map
                    (fun (KeyValue (AtomRef (alias, atomPath) as atomRef, lastValue)) ->
                        task {
                            let! result = readFile rootPath atomRef

                            let getLocals () =
                                $"alias={alias} atomPath={atomPath} lastKeys=%A{lastValue} result=%A{result} {getLocals ()}"

                            addTrace (fun () -> "[ atomWatchlist.choose ]") getLocals

                            match lastValue, result with
                            | (_, Some lastValue), Some result when lastValue = result -> ()
                            | (_, Some _), result ->
                                atomWatchlist.[atomRef] <- (Guid.newTicksGuid (), result)
                                do! sendAll (Sync.Response.GetStream (alias, atomPath, result))
                            | _ -> ()
                        })
                |> Task.WhenAll
                |> Task.ignore
        }

    [<RequireQualifiedAccess>]
    module Stream =
        let inline sendToClient rootPath (msg: Sync.Request) (hubContext: FableHub<Sync.Request, Sync.Response>) =
            update rootPath msg (Some hubContext)
            |> Async.AwaitTask
            |> AsyncSeq.init2
            |> AsyncSeq.toAsyncEnum

        type Ticker<'T, 'U when 'T: not struct and 'U: not struct> private (hub: FableHubCaller<'T, 'U>, fn) =
            let cts = new CancellationTokenSource ()

            let ticking =
                AsyncSeq.intervalMs 2000
                |> AsyncSeq.iterAsync (fun _ -> fn hub |> Async.AwaitTask)

            interface IHostedService with
                member _.StartAsync ct =
                    async { do Async.Start (ticking, cts.Token) }
                    |> fun a -> upcast Async.StartAsTask (a, cancellationToken = ct)

                member _.StopAsync ct =
                    async { do cts.Cancel () }
                    |> fun a -> upcast Async.StartAsTask (a, cancellationToken = ct)

            static member Create (services: IServiceCollection, fn) =
                services.AddHostedService<Ticker<'T, 'U>>
                    (fun s -> Ticker (s.GetRequiredService<FableHubCaller<'T, 'U>> (), fn))

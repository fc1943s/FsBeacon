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

    let inline writeFile rootPath username key value =
        task {
            try
                let path = Path.Combine (rootPath, username, key)

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

    let inline readFile rootPath username key =
        task {
            try
                let path = Path.Combine (rootPath, username, key)
                let! result = File.ReadAllTextAsync path
                return result |> Option.ofObj
            with
            | _ex ->
                //                    eprintfn $"readFile error ex={ex.Message}"
                return None
        }

    let watchlist = ConcurrentDictionary<string * string * string, string [] option> ()


    let inline fetchTableKeys rootPath username storeRoot collection =
        let path = Path.Combine (rootPath, username, $"{storeRoot}/{collection}")
        Directory.CreateDirectory path |> ignore

        Directory.EnumerateDirectories path
        |> Seq.map Path.GetFileName
        |> Seq.toArray

    let inline fetchKeys rootPath username storeRoot collection =
        let result = fetchTableKeys rootPath username storeRoot collection

        if
            watchlist.ContainsKey (username, storeRoot, collection)
            |> not
        then
            watchlist.[(username, storeRoot, collection)] <- Some [||]

        result

    let inline addDebug fn getLocals = Log.Debug $"{fn ()} {getLocals ()}"
    let inline addTrace fn getLocals = Log.Verbose $"{fn ()} {getLocals ()}"

    let inline substringTo n (str: string) =
        if str.Length > n then str |> String.substring 0 n else str

    let inline update rootPath (msg: Sync.Request) (_hubContext: FableHub<Sync.Request, Sync.Response> option) =
        task {
            let getLocals () =
                $"rootPath={rootPath} msg={string msg |> substringTo 400} {getLocals ()}"

            let inline addDebug fn getLocals =
                addDebug (fun () -> $"Hub.update {fn ()}") getLocals

            match msg with
            | Sync.Request.Connect _username ->
                addDebug (fun () -> "[ Sync.Request.Connect ]") getLocals
                return Sync.Response.ConnectResult
            | Sync.Request.Set (username, key, value) ->
                let! result = writeFile rootPath username key value
                addDebug (fun () -> "[ Sync.Request.Set ][0]") getLocals

                match key |> String.split "/" |> Array.toList with
                | storeRoot :: collection :: Guid.Valid _ :: _ ->
                    let _newKeys = fetchKeys rootPath username storeRoot collection
                    let getLocals () = $"_newKeys=%A{_newKeys} {getLocals ()}"
                    addDebug (fun () -> "[ Sync.Request.Set ][1]") getLocals
                | _ -> ()

                return Sync.Response.SetResult result
            | Sync.Request.Get (username, key) ->
                let! value = readFile rootPath username key
                let getLocals () = $"value={value} {getLocals ()}"
                addDebug (fun () -> "[ Sync.Request.Get ]") getLocals
                return Sync.Response.GetResult value
            | Sync.Request.Filter (username, storeRoot, collection) ->
                let result = fetchKeys rootPath username storeRoot collection
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

    let inline tick rootPath sendAll =
        task {
            let getLocals () =
                $"rootPath={rootPath} watchlist.Count={watchlist.Count} {getLocals ()}"

            let inline _addDebug fn getLocals =
                addDebug (fun () -> $"Hub.tick {fn ()}") getLocals

            let inline addTrace fn getLocals =
                addTrace (fun () -> $"Hub.tick {fn ()}") getLocals

            do!
                watchlist
                |> Seq.choose
                    (fun (KeyValue (collectionPath, lastValue)) ->
                        let username, storeRoot, collection = collectionPath
                        let result = fetchTableKeys rootPath username storeRoot collection

                        let getLocals () =
                            $"collectionPath={collectionPath} lastValue=%A{lastValue} result=%A{result} {getLocals ()}"

                        addTrace (fun () -> "[ watchlist.choose ]") getLocals

                        match lastValue, result with
                        | None, _ -> None
                        | Some lastValue, result when lastValue = result -> None
                        | Some _, result ->
                            watchlist.[collectionPath] <- Some result
                            Some (collectionPath, result)
                        | _ -> None)
                |> Seq.toArray
                |> Seq.map (Sync.Response.FilterStream >> sendAll)
                |> Task.WhenAll
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

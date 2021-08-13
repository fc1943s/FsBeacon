namespace FsBeacon.HubPeer

open System.Collections.Concurrent
open System.IO
open System.Threading
open FSharp.Control
open Fable.SignalR
open FsBeacon.Shared
open FSharp.Control.Tasks.V2
open System
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting


module Hub =
    let createParentDirectory path =
        Directory.CreateDirectory (Directory.GetParent(path).FullName)
        |> ignore

    let writeFile rootPath username key value =
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

    let readFile rootPath username key =
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


    let fetchTableKeys rootPath username storeRoot collection =
        let path = Path.Combine (rootPath, username, $"{storeRoot}/{collection}")
        Directory.CreateDirectory path |> ignore

        Directory.EnumerateDirectories path
        |> Seq.map Path.GetFileName
        |> Seq.toArray

    let update rootPath (msg: Sync.Request) (_hubContext: FableHub<Sync.Request, Sync.Response> option) =
        task {
            //            printfn $"Model.update() msg={msg}"

            match msg with
            | Sync.Request.Connect username ->
                printfn $"@@@ Sync.Request.Connect username={username}"
                return Sync.Response.ConnectResult
            | Sync.Request.Set (username, key, value) ->
                let! result = writeFile rootPath username key value
                //                printfn $"set {key} {value}"
//                    match hubContext with
//                    | Some _hub when result ->
//                        printfn
//                            $"Sync.Request.Set. username={username} key={key}. result=true hub.IsSome. broadcasting."

                //                        do!
//                            hub.Clients.All.Send (
//                                Sync.Response.GetResult (
//                                    key,
//                                    match value with
//                                    | String.ValidString _ -> Some value
//                                    | _ -> None
//                                )
//                            )
//                    | _ -> ()

                return Sync.Response.SetResult result
            | Sync.Request.Get (username, key) ->
                let! value = readFile rootPath username key
                //                printfn $"get username={username} key={key} value={value}"
                return Sync.Response.GetResult value
            | Sync.Request.Filter (username, storeRoot, collection) ->
                let result = fetchTableKeys rootPath username storeRoot collection
                watchlist.[(username, storeRoot, collection)] <- Some result
                printfn $"Sync.Request.Filter username={username} collection={collection} result={result.Length}"
                return Sync.Response.FilterResult result

        //        let update2 msg hubContext =
//            asyncSeq {
//                update msg hubContext
//            }
//            |> AsyncSeq.toAsyncEnum
        }

    let invoke rootPath (msg: Sync.Request) _ = update rootPath msg None

    let send rootPath (msg: Sync.Request) (hubContext: FableHub<Sync.Request, Sync.Response>) =
        task {
            let! response = update rootPath msg (Some hubContext)
            do! hubContext.Clients.Caller.Send response
        }

    [<RequireQualifiedAccess>]
    module Stream =

        let sendToClient rootPath (msg: Sync.Request) (hubContext: FableHub<Sync.Request, Sync.Response>) =
            update rootPath msg (Some hubContext)
            |> Async.AwaitTask
            |> AsyncSeq.init2
            |> AsyncSeq.toAsyncEnum

        type Ticker<'T, 'U when 'T: not struct and 'U: not struct> private (hub: FableHubCaller<'T, 'U>, fn) =
            let cts = new CancellationTokenSource ()

            let ticking =
                AsyncSeq.intervalMs 500
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

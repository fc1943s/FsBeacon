namespace FsBeacon.HubPeer

open System.IO
open System.Threading.Tasks
open Argu
open FSharp.Control
open Fable.SignalR
open FsBeacon.Shared
open Microsoft.Extensions.Logging
open FSharp.Control.Tasks.V2
open Saturn


module Main =
    let getApp port rootPath =
        application {
            use_signalr (
                configure_signalr {
                    endpoint Sync.endpoint
                    send (Hub.send rootPath)
                    invoke (Hub.invoke rootPath)
                    stream_from (Hub.Stream.sendToClient rootPath)
                    //                        use_messagepack
                    //                        with_log_level LogLevel.Trace
                    with_hub_options (fun options -> options.EnableDetailedErrors <- true)

                    with_after_routing
                        (fun _applicationBuilder ->
                            printfn "saturn.with_after_routing()"
                            _applicationBuilder)

                    with_before_routing
                        (fun _applicationBuilder ->
                            printfn "saturn.with_before_routing()"
                            _applicationBuilder)

                    with_on_disconnected (fun ex _hub -> task { printfn $"saturn.with_on_disconnected() ex={ex}" })

                    with_on_connected
                        (fun _hub ->
                            task {
                                //                                    let! result = Model.send Sync.Request.Connect hub
                                printfn "saturn.with_on_connected()"
                            //                                    return result
                            })
                //                                    return result
                }
            )

            //                config
            use_cors
                "cors"
                (fun corsBuilder ->
                    corsBuilder
                        .AllowCredentials()
                        .AllowAnyHeader()
                        .WithOrigins [|
                            "https://localhost:33929"
                            "https://localhost:33922"
                            "https://localhost:9769"
                            "https://localhost:9762"
                        |]
                    |> ignore)

            url $"https://0.0.0.0:{port}/"
            use_gzip
            disable_diagnostics
            use_developer_exceptions
            memory_cache
            no_router

            service_config
                (fun serviceCollection ->
                    Hub.Stream.Ticker.Create (
                        serviceCollection,
                        fun (hub: FableHubCaller<Sync.Request, Sync.Response>) ->
                            task {
                                do!
                                    Hub.watchlist
                                    |> Seq.choose
                                        (fun (KeyValue (collectionPath, lastValue)) ->
                                            let username, storeRoot, collection = collectionPath
                                            let result = Hub.fetchTableKeys rootPath username storeRoot collection

                                            match lastValue, result with
                                            | None, _ -> None
                                            | Some lastValue, result when lastValue = result -> None
                                            | Some _, result ->
                                                Hub.watchlist.[collectionPath] <- Some result
                                                Some (collectionPath, result)
                                            | _ -> None)
                                    |> Seq.toArray
                                    |> Seq.map (Sync.Response.FilterStream >> hub.Clients.All.Send)
                                    |> Task.WhenAll
                            }
                    ))

            logging
                (fun logging ->
                    //                        logging.SetMinimumLevel LogLevel.Debug |> ignore
                    logging.SetMinimumLevel LogLevel.Debug |> ignore

                    //                        logging.AddFilter ("Microsoft.", LogLevel.Warning)
//                        |> ignore
                    )

            force_ssl
        //                                    return result
        }

module Args =
    type Arguments =
        | [<Mandatory>] Root_Path of string
        | [<Mandatory>] Port of int
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Root_Path _ ->
                    "Where data will be stored in the format of $root_path/$username/$atomPath/$timestampHash."
                | Port _ -> "Port to serve the signalr websocket server."


module Program =
    [<EntryPoint>]
    let main argv =
        let args = Startup.parseArgs argv
        let port = args.GetResult Args.Port

        let rootPath =
            args.GetResult Args.Root_Path
            |> System.Environment.ExpandEnvironmentVariables
            |> Path.GetFullPath

        printfn $"starting app. port={port} rootPath={rootPath}"
        run (Main.getApp port rootPath)
        0

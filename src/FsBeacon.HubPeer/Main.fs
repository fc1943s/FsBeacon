namespace FsBeacon.HubPeer

open FsClr
open FsCore
open System.IO
open Argu
open Fable.SignalR
open FsBeacon.Shared
open Microsoft.Extensions.Logging
open FSharp.Control.Tasks.V2
open Saturn


module Main =
    let minimumLogLevel = LogLevel.Information

    let inline loggingFn (logging: ILoggingBuilder) =
        logging.SetMinimumLevel minimumLogLevel |> ignore
    //  logging.AddFilter ("Microsoft.", LogLevel.Warning)
    //  |> ignore

    let inline getApp port rootPath =
        application {
            url $"https://0.0.0.0:{port}/"
            use_gzip
            disable_diagnostics
            use_developer_exceptions
            memory_cache
            no_router
            logging loggingFn
            force_ssl

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

            service_config
                (fun serviceCollection ->
                    serviceCollection
                    |> Stream.withTicker
                        (fun (hub: FableHubCaller<Sync.Request, Sync.Response>) ->
                            HubServer.tick rootPath hub.Clients.All.Send)
                    |> Stream.withFileWatcher
                        rootPath
                        (fun (hub: FableHubCaller<Sync.Request, Sync.Response>, change) ->
                            HubServer.fileEvent rootPath hub.Clients.All.Send change))

            use_signalr (
                configure_signalr {
                    endpoint Sync.endpoint
                    send (HubServer.send rootPath)
                    invoke (HubServer.invoke rootPath)
                    stream_from (Stream.sendToClient HubServer.update rootPath)
                    //                        use_messagepack
                    with_log_level minimumLogLevel

                    with_hub_options
                        (fun options ->
                            options.MaximumReceiveMessageSize <- 5L * 1024L * 1024L
                            options.MaximumParallelInvocationsPerClient <- 32
                            options.EnableDetailedErrors <- true)

                    with_after_routing
                        (fun _applicationBuilder ->
                            let getLocals () =
                                $"_applicationBuilder=?obj {getLocals ()}"

                            Logger.logInfo (fun () -> "Main.getApp / use_signalr.with_after_routing") getLocals
                            _applicationBuilder)

                    with_before_routing
                        (fun _applicationBuilder ->
                            let getLocals () =
                                $"_applicationBuilder=?obj {getLocals ()}"

                            Logger.logInfo (fun () -> "Main.getApp / use_signalr.with_before_routing") getLocals
                            _applicationBuilder)

                    with_on_disconnected
                        (fun ex _hub ->
                            task {
                                let getLocals () = $"ex={ex} {getLocals ()}"
                                Logger.logInfo (fun () -> "Main.getApp / use_signalr.with_on_disconnected") getLocals
                            })

                    with_on_connected
                        (fun _hub ->
                            task {
                                // let! result = Model.send Sync.Request.Connect hub
                                let getLocals () = $"{getLocals ()}"
                                Logger.logInfo (fun () -> "Main.getApp / use_signalr.with_on_connected") getLocals
                            })
                }
            )
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
        Logger.init ()

        let args = Cli.parseArgs argv

        let getLocals () = $"args={args} {getLocals ()}"
        Logger.logInfo (fun () -> "Program.main") getLocals

        let port = args.GetResult Args.Port

        let rootPath =
            args.GetResult Args.Root_Path
            |> System.Environment.ExpandEnvironmentVariables
            |> Path.GetFullPath

        let app = Main.getApp port rootPath
        run app
        0

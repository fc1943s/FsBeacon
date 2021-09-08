namespace FsBeacon.HubPeer

open FsCore
open Serilog.Events
open Serilog.Sinks.SpectreConsole
open Giraffe.SerilogExtensions
open Serilog
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
        //                        logging.SetMinimumLevel LogLevel.Debug |> ignore
        logging.SetMinimumLevel minimumLogLevel |> ignore

    //                        logging.AddFilter ("Microsoft.", LogLevel.Warning)
    //                        |> ignore

    //    SerilogAdapter.Enable ()
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
            //            app_config (fun x -> SerilogAdapter.Enable x.)
            service_config
                (fun serviceCollection ->
                    serviceCollection
                    |> HubServer.Stream.withTicker
                        (fun (hub: FableHubCaller<Sync.Request, Sync.Response>) ->
                            HubServer.tick rootPath hub.Clients.All.Send)
                    |> HubServer.Stream.withFileWatcher
                        rootPath
                        (fun (_hub: FableHubCaller<Sync.Request, Sync.Response>, change) ->
                            async {
                                let getLocals () = $"change={change} {getLocals ()}"
                                Logger.logDebug (fun () -> "service_config withFileWatcher") getLocals
                            }))

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

            use_signalr (
                configure_signalr {
                    endpoint Sync.endpoint
                    send (HubServer.send rootPath)
                    invoke (HubServer.invoke rootPath)
                    stream_from (HubServer.Stream.sendToClient rootPath)
                    //                        use_messagepack
                    with_log_level minimumLogLevel

                    with_hub_options
                        (fun options ->
                            options.MaximumReceiveMessageSize <- 5L * 1024L * 1024L
                            options.MaximumParallelInvocationsPerClient <- 32
                            options.EnableDetailedErrors <- true)

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
        Log.Logger <-
            LoggerConfiguration()
                .Destructure.FSharpTypes()
                .Enrich.FromLogContext()
                .MinimumLevel.Verbose()
                .WriteTo.Console()
                .WriteTo
                .spectreConsole(
                    "{Timestamp:HH:mm:ss} [{Level:u4}] {Message:lj}{NewLine}{Exception}",
                    minLevel = LogEventLevel.Verbose
                )
                .CreateLogger ()

        let args = Startup.parseArgs argv
        let port = args.GetResult Args.Port

        let rootPath =
            args.GetResult Args.Root_Path
            |> System.Environment.ExpandEnvironmentVariables
            |> Path.GetFullPath

        Log.Information $"Hub.fs={Hub.fs} Hub.fsx={Hub.fsx}"

        Log.Information $"starting app. port={port} rootPath={rootPath}"
        let app = Main.getApp port rootPath
        //        SerilogAdapter.Enable
        run app
        0

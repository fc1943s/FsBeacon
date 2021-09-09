namespace FsBeacon.HubPeer

open System
open FsClr
open FsCore
open FSharp.Control
open System.Threading
open Fable.SignalR
open FsBeacon.Shared
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting


module Stream =
    let inline sendToClient update rootPath (msg: Sync.Request) (hubContext: FableHub<Sync.Request, Sync.Response>) =
        update rootPath msg (Some hubContext)
        |> Async.AwaitTask
        |> Async.initAsyncSeq
        |> AsyncSeq.toAsyncEnum

    type HubService<'Request, 'Response when 'Request: not struct and 'Response: not struct>
        (
            hub: FableHubCaller<'Request, 'Response>,
            fn,
            dispose
        ) =
        let cts = new CancellationTokenSource ()

        interface IHostedService with
            member _.StartAsync ct =
                async { Async.Start (fn hub, cts.Token) }
                |> Async.startAsTask ct

            member _.StopAsync ct =
                async {
                    dispose ()
                    cts.Cancel ()
                }
                |> Async.startAsTask ct

    type Ticker (hub, fn, dispose) =
        inherit HubService<Sync.Request, Sync.Response> (hub, fn, dispose)

        static member Create (services: IServiceCollection, fn, dispose) =
            services.AddHostedService
                (fun (serviceProvider: IServiceProvider) ->
                    let hub = serviceProvider.GetRequiredService<FableHubCaller<Sync.Request, Sync.Response>> ()
                    Ticker (hub, fn, dispose))

    type FileWatcher (hub, fn, dispose) =
        inherit HubService<Sync.Request, Sync.Response> (hub, fn, dispose)

        static member Create (services: IServiceCollection, fn, dispose) =
            services.AddHostedService
                (fun (serviceProvider: IServiceProvider) ->
                    let hub = serviceProvider.GetRequiredService<FableHubCaller<Sync.Request, Sync.Response>> ()
                    FileWatcher (hub, fn, dispose))

    let inline withTicker fn serviceCollection =
        Ticker.Create (
            serviceCollection,
            (fun hub ->
                AsyncSeq.intervalMs 2000
                |> AsyncSeq.iterAsync
                    (fun date ->
                        let getLocals () = $"ticks={date.Ticks} {getLocals ()}"
                        Logger.logDebug (fun () -> "Stream.withTicker / onTick") getLocals
                        fn hub |> Async.AwaitTask)),
            fun () -> Logger.logDebug (fun () -> "Stream.withTicker / dispose") getLocals
        )

    let inline withFileWatcher rootPath fn serviceCollection =
        let watch, disposable = FileSystem.watch rootPath
        let getLocals () = $"{getLocals ()}"

        FileWatcher.Create (
            serviceCollection,
            (fun hub ->
                watch
                |> AsyncSeq.iterAsync
                    (fun (ticks, event) ->
                        async {
                            let getLocals () = $"ticks={ticks} event={event} {getLocals ()}"
                            Logger.logTrace (fun () -> "Stream.withFileWatcher / fn") getLocals
                            fn (hub, ticks, event)
                        })),
            (fun () ->
                Logger.logDebug (fun () -> "Stream.withWileWatcher / dispose") getLocals
                disposable.Dispose ())
        )

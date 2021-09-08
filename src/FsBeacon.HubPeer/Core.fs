namespace FsBeacon.HubPeer

open System
open System.Threading.Tasks
open FSharp.Control
open FSharp.Control.Tasks.V2
open FsCore
open System.Reactive.Linq


module Async =
    let inline startAsTask ct fn : Task =
        upcast Async.StartAsTask (fn, cancellationToken = ct)


    let inline map fn op =
        async {
            let! x = op
            let value = fn x
            return value
        }

    let inline runWithTimeout timeout fn =
        try
            Async.RunSynchronously (fn, timeout)
        with
        | :? TimeoutException -> printfn $"Async timeout reached ({timeout})"
        | e -> raise e

    let inline initAsyncSeq op = AsyncSeq.initAsync 1L (fun _ -> op)


module AsyncSeq =
    let inline subscribeEvent (event: IEvent<'H, 'A>) map =
        Observable
            .FromEventPattern<'H, 'A>(event.AddHandler, event.RemoveHandler)
            .Select (fun event -> DateTime.Now.Ticks, (map event.EventArgs))
        |> AsyncSeq.ofObservableBuffered


module Task =
    let inline ignore (t: Task<unit []>) =
        task {
            let! _tasks = t
            ()
        }

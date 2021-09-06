namespace FsBeacon.HubPeer

open System.Threading.Tasks
open FSharp.Control
open FSharp.Control.Tasks.V2


module Async =
    let inline startAsTask ct fn : Task =
        upcast Async.StartAsTask (fn, cancellationToken = ct)


    let inline map fn op =
        async {
            let! x = op
            let value = fn x
            return value
        }

    let inline initAsyncSeq op = AsyncSeq.initAsync 1L (fun _ -> op)


module AsyncSeq =
    let inline forwardEvent event fn =
        AsyncSeq.unfoldAsync
            (fun () ->
                Async.FromContinuations
                    (fun (res, err, _c) ->
                        try
                            fn (fun args -> res (event args))
                        with
                        | ex -> err ex)
                |> Async.map (fun change -> Some (change, ())))
            ()


module Task =
    let inline ignore (t: Task<unit []>) =
        task {
            let! _tasks = t
            ()
        }

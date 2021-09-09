#r "nuget: FSharp.Control.AsyncSeq"
#r "nuget: Hopac"
#r "nuget: FsCheck"
#r "nuget: TaskBuilder.fs"
#r "nuget: FsCore, 0.0.1-alpha.12"
#r "nuget: XPlot.Plotly, 4.0.3"
#r "nuget: XPlot.Plotly.Interactive, 4.0.3"

//#load "Hub.fs"

//open FsBeacon.HubPeer
open FSharp.Control
//open System
//open XPlot.Plotly

// #i "nuget:https://nuget.pkg.github.com/fc1943s/index.json" // username: "%GITHUB_ACTOR%" password: "%GITHUB_READ_PACKAGES_TOKEN%"
// #i "nuget:https://pkgs.dev.azure.com/fc1943s/public/_packaging/dotnet-tools/nuget/v3/index.json"
//#r "nuget: FSharp.Core, 5.0"
// #r "./bin/Debug/net6.0/FSharp.Control.AsyncSeq.dll"
// open System
// open System.Threading
// open Hopac.Extensions
// open Hopac.Infixes
// open Hopac


// #i "nuget:https://pkgs.dev.azure.com/dnceng/public/_packaging/dotnet-tools/nuget/v3/index.json"

//#r "nuget: System.CommandLine"
//#r "nuget: FSharp.Data, 2.2.0"

//open System.Threading.Tasks
//open FSharp.Control

let fsx = 2

//let load1 () = FileSystem.watch "."

//#time "on"
//
//let intervalMs id (periodMs: int) =
//    asyncSeq {
//        yield (id, DateTime.UtcNow)
//
//        while true do
//            do! Async.Sleep periodMs
//            yield (id, DateTime.UtcNow)
//    }
//
//let either: AsyncSeq<string * DateTime> = AsyncSeq.merge (intervalMs "X" 15) (intervalMs "Y" 250)
//
//open System.Collections.Concurrent
//let values = ConcurrentDictionary<int64, string> ()
//
//let iter () =
//    either
//    |> AsyncSeq.iterAsync (fun (id, date) -> async { values.[date.Ticks] <- id })
//
//try
//    Async.RunSynchronously (iter (), 1000)
//with
//| :? TimeoutException -> ()
//| e -> raise e
//
//printfn $"values=%A{values}"
//
//let plot =
//    values
//    |> Seq.map (fun (KeyValue (ticks, id)) -> ticks, id)
//    |> Seq.groupBy snd
//    |> Seq.map
//        (fun (id, items) ->
//            let items =
//                items
//                |> Seq.map fst
//                |> Seq.map (fun n -> n)
//                |> Seq.toList
//
//            Scatter (
//                x = items,
//                y = List.init items.Length (fun _i -> id),
//                mode = "lines+markers",
//                name = "id",
//                line = Line (shape = "linear")
//            ))
//    |> Chart.Plot
//
//plot
//
//#time "off"

//let fn (hub, change) = async {
//    printfn $"change={change}"
//}
//let run () =
//     events
//     |> AsyncSeq.iterAsync (fun change ->
//          let getLocals () = $"change={change} {getLocals ()}"
//          fn ((), change))
//     |> Async.RunSynchronously

#time "on"

async {
    do! Async.Sleep 1000
    printfn "fsx body after 1s"
}
|> Async.RunSynchronously

#time "off"

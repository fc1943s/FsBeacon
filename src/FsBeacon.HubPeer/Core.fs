namespace FsBeacon.HubPeer

open System
open System.Reflection
open System.Threading.Tasks
open Argu
open FSharp.Control
open FSharp.Control.Tasks.V2


module Task =
    let ignore (t: Task<unit []>) =
        task {
            let! _tasks = t
            ()
        }

module AsyncSeq =
    let inline init2 x = AsyncSeq.initAsync 1L (fun _ -> x)


module Startup =
    let inline parseArgs<'T when 'T :> IArgParserTemplate> args =
        let errorHandler =
            ProcessExiter (
                colorizer =
                    function
                    | ErrorCode.HelpText -> None
                    | _ -> Some ConsoleColor.Red
            )

        let parser =
            ArgumentParser.Create<'T> (
                programName =
                    Assembly.GetEntryAssembly().GetName().Name
                    + ".exe",
                errorHandler = errorHandler
            )

        parser.ParseCommandLine args

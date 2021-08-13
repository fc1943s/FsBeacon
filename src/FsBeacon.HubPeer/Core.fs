namespace FsBeacon.HubPeer

open System
open System.Reflection
open Argu
open FSharp.Control


module AsyncSeq =
    let init2 x = AsyncSeq.initAsync 1L (fun _ -> x)

module Startup =
    let parseArgs<'T when 'T :> IArgParserTemplate> args =
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

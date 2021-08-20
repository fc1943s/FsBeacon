namespace FsStore.Store

open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs
open FsStore.Bindings


[<AutoOpen>]
module SyncUnsubscribe =
    module Store =
        let inline syncUnsubscribe getDebugInfo gunAtomNode subscription success =
            match subscription with
            | Some ticks when DateTime.ticksDiff ticks < 1000. ->
                Logger.logTrace
                    (fun () ->
                        $"[syncUnsubscribe] skipping unsubscribe. jotai resubscribe glitch.
                            gunAtomNode={gunAtomNode} {getDebugInfo ()} ")
            | Some _ ->
                match gunAtomNode with
                | Some (key, gunAtomNode: Gun.Types.IGunChainReference) ->

                    Profiling.addCount $"{key} unsubscribe"

                    Logger.logTrace
                        (fun () ->
                            $"[syncUnsubscribe] {key} (######## actually skipped)
                                gunAtomNode={gunAtomNode} {getDebugInfo ()} ")

                    gunAtomNode.off () |> ignore
                    success ()
                | None ->
                    Logger.logTrace
                        (fun () ->
                            $"[syncUnsubscribe] skipping unsubscribe, no gun atom node.
                                gunAtomNode={gunAtomNode} {getDebugInfo ()} ")

            | None ->
                Logger.logTrace
                    (fun () ->
                        $"[syncUnsubscribe] skipping unsubscribe. no last subscription found.
                                gunAtomNode={gunAtomNode} {getDebugInfo ()} ")

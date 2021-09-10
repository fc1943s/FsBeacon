namespace FsBeacon.Template.Components

open FsStore
open FsCore
open Fable.React
open Feliz
open FsJs
open FsStore.Hooks
open FsStore.State
open FsBeacon.Template.State
open FsUi.State


module HydrateCoreContainer =
    [<ReactComponent>]
    let rec HydrateCoreContainer () =
        Profiling.addTimestamp
            (fun () -> $"{nameof FsBeacon} | HydrateCoreContainer [ render ] hydrate trace from now on ")
            getLocals

        //        if Atoms.showDebug?init
//           |> Option.ofObjUnbox
//           |> Option.isNone then
//            failwith "invalid Atoms.showDebug init"
//
//        if Atoms.logLevel?init
//           |> Option.ofObjUnbox
//           |> Option.isNone then
//            failwith "invalid Atoms.logLevel init"

        //        Jotai.jotaiUtils.useHydrateAtoms [|
//            unbox Atoms.showDebug, unbox true
//            unbox Atoms.logLevel, unbox Logger.LogLevel.Trace
//        |]

        Store.useHashedEffectOnce
            (nameof HydrateCoreContainer)
            (fun _ setter ->
                promise {
                    Atom.set setter Atoms.showDebug true
                    Atom.set setter Atoms.logLevel Logger.LogLevel.Trace
                    Dom.globalDebug.Set true
                })

        nothing

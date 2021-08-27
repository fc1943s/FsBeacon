namespace FsStore.Store

open System
open System.Collections.Generic
open FsStore
open FsStore.Bindings.Gun.Types
open FsStore.Model
open FsStore.State
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs


[<AutoOpen>]
module SyncEngine =
    module Store =
        [<RequireQualifiedAccess>]
        type AdapterSubscription =
            | Gun of IGunChainReference
            | Hub

        [<RequireQualifiedAccess>]
        type ValueKeyOperation =
            | Add
            | Remove

        let adapterValueTimestampMap = Dictionary<Atom.AdapterType, int64> ()
        let adapterValueMap = Dictionary<Atom.AdapterType, int64> ()

        let valueKeyOperation = DateTime.Now.Ticks, Atom.AdapterType.Gun, ValueKeyOperation.Add

        type SyncEngine<'T> (defaultValue: 'T, mapGunAtomNode) =
            let internalAtom = Atom.atomFamilyAtom (fun _alias -> defaultValue)
            let mutable lastAtomPath = None
            let mutable lastStore = None
            let mutable lastAlias = None
            let mutable lastGunOptions = None
            let mutable lastGunAtomNode = None
            let mutable lastHub = None
            let mutable subscription = None

            let mutable lastSubscribeParameters: (('T -> unit) -> SubscriptionId -> Fable.Core.JS.Promise<IDisposable option>) * ('T -> unit) =
                (fun _ _ -> failwith "no lastSubscribeParameters"), (fun _ -> failwith "lastSubscribeParameters")

            let mutable getDebugSummaryCount = 0

            let getDebugSummary () =
                let result =
                    Json.encodeWithNull
                        {|
                            lastAtomPath = lastAtomPath
                            lastAlias = lastAlias
                            lastGunOptions = lastGunOptions
                            subscription = subscription
                            getDebugSummaryCount = getDebugSummaryCount
                        |}

                getDebugSummaryCount <- getDebugSummaryCount + 1
                result

            let getStore () =
                lastStore
                |> Option.defaultWith (fun () -> failwith "invalid store")

            let subscribe () =
                promise {
                    let subscriptionId = SubscriptionId.NewId ()
                    let subscribe, callback = lastSubscribeParameters

                    Logger.logTrace (fun () -> $"SyncEngine.subscribe. before subscribe promise. {getDebugSummary ()}")

                    let! disposable = subscribe callback subscriptionId

                    Logger.logTrace (fun () -> $"SyncEngine.subscribe. after subscribe promise. {getDebugSummary ()}")
                    //                            |> Promise.bind
                    //                                (fun disposablePromise ->
                    //                                    disposablePromise
                    //                                    |> Option.defaultWith
                    //                                        (fun () -> Store.newHashedDisposable (subscriptionId |> SubscriptionId.Value)))

                    subscription <- Some (subscriptionId, disposable)

                    if disposable.IsSome then
                        Profiling.addCount (fun () -> $"{nameof FsStore} | @ {lastAtomPath}")

                        //                            let! events =
//                                StoreEngine.consumeMessages
//                                    (Dom.Global.get "update" (fun _ _ _ _ ->
//                                        eprintfn "warning: update is still not found"
//                                        promise { () }
//                                        ))
//                                    getter
//                                    setter
//                                    null
//                                    (Message.Command Command.Subscribe
//                                     |> List.singleton)
                        let events = null

                        Logger.logTrace
                            (fun () -> $"SyncEngine.subscribe. consumed. events={events} {getDebugSummary ()}")
                        //                            gunAtomNodeFromAtomPath getter lastAlias lastAtomPath
//                            |> Option.map (mapGunAtomNode |> Option.defaultValue id)
                        //                            lastGunAtomNode

                        if (lastAtomPath
                            |> Option.defaultValue (AtomPath "")
                            |> AtomPath.Value)
                            .Contains "/pub" then
                            Profiling.addCount (fun () -> $"{nameof FsStore} | @@> {getDebugSummary ()}")
                    else
                        Logger.logTrace
                            (fun () ->
                                $"SyncEngine.subscribe. no disposable returned from subscribe. {getDebugSummary ()}")
                //                        Logger.logTrace (fun () -> $"SyncEngine.debouncedSubscribe. this={Json.encodeWithNull this} ")
                }

            let debouncedSubscribe = Js.debounce (fun () -> subscribe () |> Promise.start) 0

            member this.GetDebugSummary = getDebugSummary
            member this.GetUserAtom<'T> () = internalAtom lastAlias
            member this.GetAtomPath () = lastAtomPath
            member this.GetAlias () = lastAlias
            member this.GetGunOptions () = lastGunOptions
            member this.GetStore = getStore

            member this.GetGunAtomNode () =
                match lastStore with
                | Some (getter, _) ->
                    Atom.get getter (Selectors.Gun.gunAtomNode (lastAlias, lastAtomPath.Value))
                    |> Option.map (mapGunAtomNode |> Option.defaultValue id)
                | None -> lastGunAtomNode

            member this.GetHub () = lastHub
            member this.GetSubscription () = subscription


            member this.Subscribe (subscribe, callback) =
                lastSubscribeParameters <- subscribe, callback
                //
//                Logger.logTrace
//                    (fun () -> $"SyncEngine.Subscribe. debouncing from onMount... this={Json.encodeWithNull this} ")
//
                debouncedSubscribe ()

            member this.Unsubscribe fn =

                //                | Some (ticksGuid, None) ->
//                    Logger.logTrace
//                        (fun () ->
//                            $"SyncEngine.Unsubscribe. onUnmount. skipping. has tick but no disposable. ticksGuid={ticksGuid} this={Json.encodeWithNull this}")

                match subscription with
                | Some (ticksGuid, disposable) ->
                    //                    Logger.logTrace
//                        (fun () ->
//                            $"SyncEngine.Unsubscribe. onUnmount. unsubbing. ticksGuid={ticksGuid} this={Json.encodeWithNull this}")

                    subscription <- None

                    match disposable with
                    | Some disposable ->
                        (disposable: IDisposable).Dispose ()

                        Profiling.removeCount (fun () -> $"@ {this.GetAtomPath ()}")

                        if (lastAtomPath
                            |> Option.defaultValue (AtomPath "")
                            |> AtomPath.Value)
                            .Contains "/pub" then
                            Profiling.addCount (fun () -> $"{nameof FsStore} | @@< {getDebugSummary ()}")

                        fn ticksGuid
                    | None ->
                        Logger.logTrace
                            (fun () ->
                                $"SyncEngine.Unsubscribe. onUnmount. skipping. has tick but no disposable. ticksGuid={ticksGuid} this={Json.encodeWithNull this}")
                | _ -> ()
            //                    Logger.logTrace
//                        (fun () -> $"SyncEngine.Unsubscribe. skipping unsub. this={Json.encodeWithNull this}")


            member this.SetProviders getter atom =
                if lastAtomPath.IsNone then
                    lastAtomPath <-
                        Atom.query (AtomReference.Atom atom)
                        |> StoreAtomPath.AtomPath
                        |> Some

                if lastStore.IsNone then lastStore <- Atom.get getter Selectors.store

                lastAlias <- Atom.get getter Selectors.Gun.alias
                lastGunOptions <- Some (Atom.get getter Atoms.gunOptions)
                Logger.State.lastLogger <- Atom.get getter Selectors.logger
                lastGunAtomNode <- this.GetGunAtomNode ()

                match lastAtomPath, lastGunAtomNode with
                | Some _, Some _ ->
                    match subscription with
                    | Some (_, None) ->
                        Logger.logTrace
                            (fun () ->
                                $"SyncEngine.SetProviders. subscription and gun node present but no disposable. subscribing. this={Json.encodeWithNull this}")

                        debouncedSubscribe ()
                    //                        debouncedSubscribe ()
                    | _ ->
                        Logger.logTrace
                            (fun () -> $"SyncEngine.SetProviders. gun node present.  this={Json.encodeWithNull this}")

                    lastHub <- Atom.get getter Selectors.Hub.hub
                | _ -> ()

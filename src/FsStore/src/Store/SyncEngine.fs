namespace FsStore.Store

open System
open System.Collections.Generic
open FsStore
open FsStore.BaseStore.Store
open FsStore.Bindings
open FsStore.Bindings.Gun.Types
open FsStore.Model
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

        let adapterValueTimestampMap = Dictionary<AdapterType, int64> ()
        let adapterValueMap = Dictionary<AdapterType, int64> ()

        let valueKeyOperation = DateTime.Now.Ticks, AdapterType.Gun, ValueKeyOperation.Add

        type SyncEngine<'T> (defaultValue: 'T, mapGunAtomNode) =
            let internalAtom = Jotai.jotaiUtils.atomFamily (fun _alias -> Jotai.jotai.atom defaultValue) Object.compare
            let mutable lastAtomPath = None
            let mutable lastAccessors = None
            let mutable lastAlias = None
            let mutable lastGunOptions = None
            let mutable lastGunAtomNode = None
            let mutable lastHub = None
            let mutable subscription = None

            let mutable lastSubscribeParameters: (('T -> unit) -> SubscriptionId -> Fable.Core.JS.Promise<IDisposable option>) * ('T -> unit) =
                (fun _ _ -> failwith "no lastSubscribeParameters"), (fun _ -> failwith "lastSubscribeParameters")

            let subscribe () =
                promise {
                    let subscriptionId = SubscriptionId.NewId ()
                    let subscribe, callback = lastSubscribeParameters

                    Logger.logTrace
                        (fun () ->
                            $"SyncEngine.subscribe. before subscribe promise. lastAtomPath={lastAtomPath} subscription={subscription} lastGunAtomNode={lastGunAtomNode}")

                    let! disposable = subscribe callback subscriptionId

                    Logger.logTrace
                        (fun () ->
                            $"SyncEngine.subscribe. after subscribe promise. lastAtomPath={lastAtomPath} subscription={subscription} lastGunAtomNode={lastGunAtomNode}")
                    //                            |> Promise.bind
                    //                                (fun disposablePromise ->
                    //                                    disposablePromise
                    //                                    |> Option.defaultWith
                    //                                        (fun () -> Store.newHashedDisposable (subscriptionId |> SubscriptionId.Value)))

                    subscription <- Some (subscriptionId, disposable)

                    if disposable.IsSome then
                        Profiling.addCount $"@{lastAtomPath}"
                    else
                        Logger.logTrace
                            (fun () ->
                                $"SyncEngine.subscribe. no disposable returned from subscribe. lastAtomPath={lastAtomPath} subscription={subscription} lastGunAtomNode={lastGunAtomNode}")
                //                        Logger.logTrace (fun () -> $"SyncEngine.debouncedSubscribe. this={Json.encodeWithNull this} ")
                }

            let debouncedSubscribe = Js.debounce (fun () -> subscribe () |> Promise.start) 0

            member this.GetUserAtom<'T> () = internalAtom lastAlias
            member this.GetAtomPath () = lastAtomPath
            member this.GetAccessors () = lastAccessors
            member this.GetAlias () = lastAlias
            member this.GetGunOptions () = lastGunOptions

            member this.GetGunAtomNode () =
                match lastAccessors with
                | Some (getter, _) ->
                    gunAtomNodeFromAtomPath getter lastAlias lastAtomPath
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
                        Profiling.removeCount $"@{this.GetAtomPath ()}"
                        (disposable: IDisposable).Dispose ()
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
                    lastAtomPath <- Internal.queryAtomPath (AtomReference.Atom atom)

                if lastAccessors.IsNone then
                    lastAccessors <- Store.value getter Selectors.atomAccessors

                lastAlias <- Store.value getter Selectors.Gun.alias
                lastGunOptions <- Some (Store.value getter Atoms.gunOptions)
                Logger.State.lastLogger <- Store.value getter Selectors.logger
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

                    lastHub <- Store.value getter Selectors.Hub.hub
                | _ -> ()

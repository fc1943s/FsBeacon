namespace FsStore.Store

open System
open FsStore
open FsStore.Bindings
open FsStore.Model
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs


[<AutoOpen>]
module SyncEngine =
    module Store =
        type SyncEngine<'T> (defaultValue: 'T, mapGunAtomNode) =
            let internalAtom = Jotai.jotaiUtils.atomFamily (fun _alias -> Jotai.jotai.atom defaultValue) Object.compare
            let mutable lastAtomPath = None
            let mutable lastAccessors = None
            let mutable lastAlias = None
            let mutable lastGunOptions = None
            let mutable lastGunAtomNode = None
            let mutable lastHub = None
            let mutable subscription = None

            let debouncedSubscribe =
                Js.debounce
                    (fun (this: SyncEngine<'T>, subscribe, callback: 'T -> unit) ->
                        Profiling.addCount $"@{this.GetAtomPath ()}"
                        let subscriptionId = SubscriptionId.NewId ()

                        let disposable =
                            subscribe callback subscriptionId
                            |> Promise.bind
                                (fun disposablePromise ->
                                    disposablePromise
                                    |> Option.defaultWith
                                        (fun () -> Store.newHashedDisposable (subscriptionId |> SubscriptionId.Value)))

                        subscription <- Some (subscriptionId, disposable)
                        //                        Logger.logTrace (fun () -> $"SyncEngine.debouncedSubscribe. this={Json.encodeWithNull this} ")
                        )
                    0

            member this.GetUserAtom<'T> () = internalAtom lastAlias
            member this.GetAtomPath () = lastAtomPath
            member this.GetAccessors () = lastAccessors
            member this.GetAlias () = lastAlias
            member this.GetGunOptions () = lastGunOptions
            member this.GetGunAtomNode () = lastGunAtomNode
            member this.GetHub () = lastHub
            member this.GetSubscription () = subscription


            member this.Subscribe (subscribe, callback) =
                //
//                Logger.logTrace
//                    (fun () -> $"SyncEngine.Subscribe. debouncing from onMount... this={Json.encodeWithNull this} ")
//
                debouncedSubscribe (this, subscribe, callback)

            member this.Unsubscribe fn =

                match subscription with
                | Some (ticksGuid, disposable) ->
                    //                    Logger.logTrace
//                        (fun () ->
//                            $"SyncEngine.Unsubscribe. onUnmount. unsubbing. ticksGuid={ticksGuid} this={Json.encodeWithNull this}")

                    promise {
                        subscription <- None
                        Profiling.removeCount $"@{this.GetAtomPath ()}"

                        let! disposable = disposable
                        (disposable: IDisposable).Dispose ()
                        fn ticksGuid
                    }
                    |> Promise.start

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

                lastGunAtomNode <-
                    Store.gunAtomNodeFromAtomPath getter lastAlias lastAtomPath
                    |> Option.map (mapGunAtomNode |> Option.defaultValue id)

                match lastAtomPath, lastGunAtomNode with
                | Some _, Some _ -> lastHub <- Store.value getter Selectors.Hub.hub
                | _ -> ()

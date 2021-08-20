namespace FsStore.Store

open FsStore
open FsStore.Model
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs


[<AutoOpen>]
module SyncEngine =
    module Store =
        type SyncEngine (mapGunAtomNode) =
            let mutable lastAtomPath = None
            let mutable lastAccessors = None
            let mutable lastAlias = None
            let mutable lastGunOptions = None
            let mutable lastGunAtomNode = None
            let mutable lastHub = None
            let mutable subscriptionCount = 0
            let mutable unsubscribeEnabled = false

            member this.GetAtomPath () = lastAtomPath
            member this.GetAccessors () = lastAccessors
            member this.GetAlias () = lastAlias
            member this.GetGunOptions () = lastGunOptions
            member this.GetGunAtomNode () = lastGunAtomNode
            member this.GetHub () = lastHub
            member this.GetSubscriptionCount () = subscriptionCount

            member this.AddSubscriptionCount () =
                subscriptionCount <- subscriptionCount + 1

            member this.RemoveSubscriptionCount () =
                subscriptionCount <- subscriptionCount - 1

            member this.SetProviders getter atom =
                unsubscribeEnabled <- true

                if lastAtomPath.IsNone then
                    lastAtomPath <- Internal.queryAtomPath (AtomReference.Atom atom)

                Profiling.addCount $"createSyncEngine.setProviders {lastAtomPath}"

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

                match lastAtomPath with
                | Some (AtomPath atomPath) ->
                    Logger.logTrace
                        (fun () ->
                            $"SyncEngine.SetProviders() atom={atom} atomPath={atomPath} this={Json.encodeWithNull this}")
                | _ -> ()

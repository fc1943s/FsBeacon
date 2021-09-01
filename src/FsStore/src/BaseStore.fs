namespace FsStore

open System.Collections.Generic
open Fable.Extras
open System
open FsStore.Model
open FsBeacon.Shared
open FsStore.State
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs
open FsStore.Bindings

#nowarn "40"


[<AutoOpen>]
module BaseStore =
    module Store =


        let testKeysCache = Dictionary<string, Set<string>> ()

        let inline splitAtomPath (AtomPath atomPath) =
            let matches =
                (JSe.RegExp @"(.*?)\/([\w-]{36})\/\w+.*?")
                    .Match atomPath
                |> Option.ofObj
                |> Option.defaultValue Seq.empty
                |> Seq.toList

            match matches with
            | _match :: root :: guid :: _key -> Some (root, guid)
            | _ -> None

        let inline newHashedDisposable (ticks: TicksGuid) =
            promise {
                Logger.logDebug (fun () -> $"BaseStore.newHashedDisposable constructor ticks={ticks}")

                return
                    Object.newDisposable
                        (fun () ->
                            Logger.logDebug (fun () -> $"BaseStore.newHashedDisposable disposing... ticks={ticks}"))
            }





        //    type Msg =
        //        | Internal of int64 option * obj
        //        | Gun of int64 * obj
        //        | Hub of int64 * obj


        type Command2 =
            | UnregisterAdapter
            | CreateUser
            | SignInAlias
            | SignInPair
            | SignOutUser


        type Event2<'T> =
            //            | AdapterRegistered
            | AdapterUnregistered
            | UserCreated
            | UserSignedIn
            | UserSignedOut
            | AtomMount
            | AtomUnmount
            | AdapterEnable
            | AdapterSubscribe
            | AdapterValue of Atom.AdapterType * 'T
            | AdapterUnsubscribe
            | AdapterDisable

        let adapterValueMap = Map<TicksGuid, Atom.AdapterType * 'T>
        type Z = Map<Atom.AdapterType, TicksGuid * Gun.EncryptedSignedValue> //selector, defaultValue
        type R = TicksGuid -> Gun.EncryptedSignedValue
        type F = Gun.EncryptedSignedValue

        //    type AckMap = Map<AdapterValue<'T>, > //selector, defaultValue

        type Y =
            | Y //of Msg
            | W

        type AtomSyncState<'T> = { Value: 'T }


        type SyncState<'TValue> () =
            let mutable lastAdapterValueMapByType: Map<Atom.AdapterType, (TicksGuid * 'TValue) option> option = None
            let mutable lastGunSubscription = None
            let mutable lastHubSubscription = None
            let mutable syncPaused = false

            member this.AdapterValueMapByType
                with get () = lastAdapterValueMapByType
                and set value = lastAdapterValueMapByType <- value

            member this.GunSubscription
                with get () = lastGunSubscription
                and set (value: int64 option) = lastGunSubscription <- value

            member this.HubSubscription
                with get () = lastHubSubscription
                and set (value: IDisposable option) = lastHubSubscription <- value

            member this.SyncPaused
                with get () = syncPaused
                and set value = syncPaused <- value






        let inline deleteRoot getter atom =
            promise {
                let alias = Atom.get getter Selectors.Gun.alias
                let storeAtomPath = Atom.query (AtomReference.Atom atom)

                let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))

                match gunAtomNode with
                | Some gunAtomNode ->
                    let! putResult = Gun.put (gunAtomNode.back ()) (unbox null)
                    Logger.logDebug (fun () -> $"Store.deleteRoot. putResult={putResult}")
                | None -> failwith "Store.deleteRoot. invalid gun atom node"

                match alias with
                | Some (Gun.Alias alias) ->
                    let hub = Atom.get getter Selectors.Hub.hub

                    match hub with
                    | Some hub ->
                        let nodes = atomPath |> AtomPath.Value |> String.split "/"

                        if nodes.Length > 3 then
                            let rootAtomPath = nodes |> Array.take 3 |> String.concat "/"
                            do! hub.sendAsPromise (Sync.Request.Set (alias, rootAtomPath, null))
                    | _ -> Logger.logDebug (fun () -> "Store.deleteRoot. invalid hub. skipping")
                | _ -> failwith "Store.deleteRoot. invalid alias"
            }

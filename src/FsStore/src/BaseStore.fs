namespace FsStore

open System.Collections.Generic
open Fable.Extras
open Fable.Core
open System
open FsStore.Model
open FsBeacon.Shared
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs
open FsStore.Bindings
open FsStore.Bindings.Jotai

#nowarn "40"


[<AutoOpen>]
module BaseStore =
    module Store =
        let inline gunAtomNodeFromAtomPath getter alias atomPath =
            match alias, atomPath with
            | Some alias, Some atomPath ->
                match Store.value getter (Selectors.Gun.gunAtomNode (alias, atomPath)) with
                | Some gunAtomNode -> Some ($">> atomPath={atomPath} alias={alias}", gunAtomNode)
                | _ -> None
            | _ -> None


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
                Logger.logDebug (fun () -> $"emptyDisposableFromTrigger() ticks={ticks}")

                return
                    Object.newDisposable
                        (fun () -> Logger.logDebug (fun () -> $"emptyDisposableFromTrigger Dispose. ticks={ticks}"))
            }

        let inline splitAtom atom = jotaiUtils.splitAtom atom





        //    type Msg =
        //        | Internal of int64 option * obj
        //        | Gun of int64 * obj
        //        | Hub of int64 * obj

        [<RequireQualifiedAccess>]
        type AdapterType =
            | Internal
            | Gun
            | Hub

        [<RequireQualifiedAccess>]
        type AdapterValue<'T> =
            | Internal of 'T
            | Gun of 'T
            | Hub of 'T

        type Event<'T> =
            | UserCreated
            | UserSignedIn
            | AdapterEnable
            | AdapterSubscribe
            | AdapterValue of AdapterValue<'T>
            | AdapterUnsubscribe
            | AdapterDisable

        type AdapterValueMap2<'T> = Map<TicksGuid, AdapterValue<'T>>
        type Z = Map<AdapterType, TicksGuid * Gun.EncryptedSignedValue> //selector, defaultValue
        type R = TicksGuid -> Gun.EncryptedSignedValue
        type F = Gun.EncryptedSignedValue

        //    type AckMap = Map<AdapterValue<'T>, > //selector, defaultValue

        type Y =
            | Y //of Msg
            | W

        type AtomSyncState<'T> = { Value: 'T }


        type SyncState<'TValue> () =
            let mutable lastAdapterValueMapByType: Map<AdapterType, (TicksGuid * 'TValue) option> option = None
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
                and set (value: unit option) = lastHubSubscription <- value

            member this.SyncPaused
                with get () = syncPaused
                and set value = syncPaused <- value






        let inline groupAdapterValueMapByType adapterValueMap =
            let newMap =
                adapterValueMap
                |> Map.toSeq
                |> Seq.map
                    (function
                    | ticks, AdapterValue.Internal value -> AdapterType.Internal, (ticks, value)
                    | ticks, AdapterValue.Gun value -> AdapterType.Gun, (ticks, value)
                    | ticks, AdapterValue.Hub value -> AdapterType.Hub, (ticks, value))
                |> Seq.groupBy fst
                |> Map.ofSeq
                |> Map.map
                    (fun _ v ->
                        v
                        |> Seq.map snd
                        |> Seq.sortByDescending fst
                        |> Seq.head)

            Reflection.unionCases<AdapterType>
            |> List.map (fun case -> case, (newMap |> Map.tryFind case))
            |> Map.ofList


        [<RequireQualifiedAccess>]
        type BatchKind =
            | Replace
            | Union

        let provider = jotai.provider

        let emptyArrayAtom = jotai.atom<obj []> [||]

        let inline waitForAll<'T> (atoms: Atom<'T> []) =
            match atoms with
            | [||] -> unbox emptyArrayAtom
            | _ -> jotaiUtils.waitForAll atoms

        let inline deleteRoot getter atom =
            promise {
                let alias = Store.value getter Selectors.Gun.alias
                let atomPath = Internal.queryAtomPath (AtomReference.Atom atom)

                let gunAtomNode = gunAtomNodeFromAtomPath getter alias atomPath

                match gunAtomNode with
                | Some (_key, gunAtomNode) ->
                    let! _putResult = Gun.put (gunAtomNode.back ()) (unbox null)
                    ()
                | None -> ()

                match alias, atomPath with
                | Some (Gun.Alias alias), Some (AtomPath atomPath) ->
                    let hub = Store.value getter Selectors.Hub.hub

                    match hub with
                    | Some hub ->
                        let nodes = atomPath |> String.split "/"

                        if nodes.Length > 3 then
                            let rootAtomPath = nodes |> Array.take 3 |> String.concat "/"
                            do! hub.sendAsPromise (Sync.Request.Set (alias, rootAtomPath, null))
                    | _ -> ()
                | _ -> ()
            }

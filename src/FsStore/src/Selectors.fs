namespace FsStore

open System.Collections.Generic
open Fable.Core
open FsCore
open FsCore.Model
open FsStore.Bindings.Gun
open FsStore.Bindings.Jotai
open FsStore.Model
open FsBeacon.Shared
open Microsoft.FSharp.Core.Operators
open FsJs
open FsStore.Bindings

#nowarn "40"



module Selectors =
    let rec deviceInfo = Store.readSelector FsStore.root (nameof deviceInfo) (fun _ -> Dom.deviceInfo)

    let rec logger =
        Store.readSelector
            FsStore.root
            (nameof logger)
            (fun getter ->
                let logLevel = Store.value getter Atoms.logLevel
                Dom.Logger.Create logLevel)

    let rec atomAccessors =
        let mutable lastValue = 0
        let valueAtom = jotai.atom lastValue
        let accessorsAtom = jotai.atom (None: (GetFn * SetFn) option)

        let getDebugInfo () =
            $"
| atomAccessors baseInfo:
lastValue={lastValue}
"

        Dom
            .Logger
            .getLogger()
            .Trace (fun () -> $"atomAccessors.constructor {getDebugInfo ()}")

        let rec valueWrapper =
            Store.selector
                FsStore.root
                (nameof valueWrapper)
                (fun getter ->
                    let result = Store.value getter valueAtom

                    Dom
                        .Logger
                        .getLogger()
                        .Trace (fun () -> $"atomAccessors.valueWrapper.get() result={result} {getDebugInfo ()}")

                    result)
                (fun getter setter newValue ->
                    Dom
                        .Logger
                        .getLogger()
                        .Trace (fun () -> $"atomAccessors.valueWrapper.set() newValue={newValue} {getDebugInfo ()}")

                    Store.set setter accessorsAtom (Some (getter, setter))
                    Store.set setter valueAtom newValue)

        valueWrapper.onMount <-
            fun setAtom ->
                Dom
                    .Logger
                    .getLogger()
                    .Trace (fun () -> $"atomAccessors.valueWrapper.onMount() lastValue={lastValue} {getDebugInfo ()}")

                lastValue <- lastValue + 1
                setAtom lastValue

                fun () ->
                    Dom
                        .Logger
                        .getLogger()
                        .Trace (fun () ->
                            $"atomAccessors.valueWrapper.onUnmount() lastValue={lastValue} {getDebugInfo ()}")

                    ()

        Store.readSelector
            FsStore.root
            (nameof atomAccessors)
            (fun getter ->
                let value = Store.value getter valueWrapper
                let accessors = Store.value getter accessorsAtom

                Dom
                    .Logger
                    .getLogger()
                    .Trace (fun () ->
                        $"atomAccessors.selfWrapper.get() value={value} accessors={accessors.IsSome} {getDebugInfo ()}")

                accessors)


    module rec Gun =
        let collection = Collection (nameof Gun)

        let rec gunPeers =
            Store.readSelector
                FsStore.root
                (nameof gunPeers)
                (fun getter ->
                    let gunOptions = Store.value getter Atoms.gunOptions

                    match gunOptions with
                    | GunOptions.Minimal -> [||]
                    | GunOptions.Sync gunPeers ->
                        gunPeers
                        |> Array.filter
                            (function
                            | GunPeer (String.ValidString _) -> true
                            | _ -> false))

        let rec gun =
            Store.readSelector
                FsStore.root
                (nameof gun)
                (fun getter ->
                    //                    let deviceInfo = Store.value getter deviceInfo
                    let gunPeers = Store.value getter gunPeers

                    let gun =
                        //                        if deviceInfo.IsTesting then
//                            Bindings.Gun.gun
//                                {
//                                    GunProps.peers = None
//                                    GunProps.radisk = Some false
//                                    GunProps.localStorage = Some false
//                                    GunProps.multicast = None
//                                }
//                        else
                        Bindings.Gun.gun
                            {
                                GunProps.peers = Some gunPeers
                                GunProps.radisk = Some true
                                GunProps.localStorage = Some false
                                GunProps.multicast = None
                            }

                    Dom.Logger.Default.Debug
                        (fun () -> $"Selectors.Gun.gun. gunPeers={gunPeers}. gun={gun} returning...")

                    gun)


        let rec gunUser =
            Store.readSelector
                FsStore.root
                (nameof gunUser)
                (fun getter ->
                    let _gunTrigger = Store.value getter Atoms.gunTrigger
                    let gun = Store.value getter gun

                    Dom.Logger.Default.Debug
                        (fun () -> $"Selectors.Gun.gunUser. keys={gun.user().__.sea |> Js.objectKeys}")

                    gun.user ())

        let rec gunNamespace =
            Store.readSelector
                FsStore.root
                (nameof gunNamespace)
                (fun getter ->
                    let _gunTrigger = Store.value getter Atoms.gunTrigger
                    let gunUser = Store.value getter gunUser

                    Dom.Logger.Default.Debug
                        (fun () -> $"Selectors.Gun.gunNamespace. gunUser.is={JS.JSON.stringify gunUser.is}")

                    gunUser :> Types.IGunNode)

        let rec alias =
            Store.readSelector
                FsStore.root
                (nameof alias)
                (fun getter ->
                    let _gunTrigger = Store.value getter Atoms.gunTrigger
                    let gunUser = Store.value getter Gun.gunUser


                    match gunUser.is with
                    | Some {
                               alias = Some (GunUserAlias.Alias (Alias (String.ValidString alias)))
                           } ->
                        Dom.Logger.Default.Debug
                            (fun () -> $"Selectors.Gun.alias. alias={alias}  keys={gunUser.__.sea |> Js.objectKeys}")

                        Some (Alias alias)
                    | _ ->
                        match gunUser.is with
                        | Some {
                                   alias = Some (GunUserAlias.GunKeys {
                                                                          priv = Some (Priv (String.ValidString _))
                                                                      })
                               } ->
                            Dom.Logger.Default.Debug
                                (fun () ->
                                    $"Selectors.Gun.alias. alias not found. keys found. user.is={gunUser.is |> Js.objectKeys}")

                            None
                        | _ ->
                            Dom.Logger.Default.Debug
                                (fun () ->
                                    $"Selectors.Gun.alias. returning none.
                                                  user.is={JS.JSON.stringify gunUser.is}")

                            None)

        let rec keys =
            Store.readSelector
                FsStore.root
                (nameof keys)
                (fun getter ->
                    let _gunTrigger = Store.value getter Atoms.gunTrigger
                    let gunUser = Store.value getter Gun.gunUser
                    Dom.Logger.Default.Debug (fun () -> $"Selectors.Gun.keys. keys={gunUser.__.sea |> Js.objectKeys}")
                    gunUser.__.sea)

        let getRecursiveNode (gunNode: Types.IGunNode) (nodes: GunNodeSlice list) getter alias =
            match nodes with
            | [] -> None
            | [ root ] -> Some (gunNode.get root)
            | nodes ->
                let lastNode = nodes |> List.last

                let parentAtomPath =
                    AtomPath (
                        nodes.[0..nodes.Length - 2]
                        |> List.map GunNodeSlice.Value
                        |> String.concat "/"
                    )

                let node = Store.value getter (gunAtomNode (alias, parentAtomPath))

                node
                |> Option.map (fun (node: Types.IGunChainReference) -> node.get lastNode)

        let rec gunAtomNode =
            Store.readSelectorFamily
                FsStore.root
                (nameof gunAtomNode)
                (fun (alias: Alias, AtomPath atomPath) getter ->
                    let gunNamespace = Store.value getter gunNamespace

                    let nodes =
                        atomPath
                        |> String.split "/"
                        |> Array.toList
                        |> List.map GunNodeSlice

                    //                    let getNodeOld () =
//                        (Some (gunNamespace.get nodes.Head), nodes.Tail)
//                        ||> List.fold
//                                (fun result node ->
//                                    result
//                                    |> Option.map (fun result -> result.get node))


                    getRecursiveNode gunNamespace nodes getter alias)

    //    let rec username = Store.atom FsStore.root (nameof username) (None: Username option)
//    let rec gunKeys = Store.atom FsStore.root (nameof gunKeys) Gun.GunKeys.Default



    module Hub =
        open Fable.SignalR

        let hubSubscriptionMap = Dictionary<string * string * string, string [] -> unit> ()


        let rec hubConnection =
            Store.readSelector
                FsStore.root
                (nameof hubConnection)
                (fun getter ->
                    let timeout = 2000

                    let hubUrl = Store.value getter Atoms.hubUrl
                    let alias = Store.value getter Gun.alias

                    Dom.Logger.Default.Debug
                        (fun () -> $"Selectors.Hub.hubConnection. start. alias={alias} hubUrl={hubUrl}")

                    match alias, hubUrl with
                    | Some (Alias (String.ValidString _)), Some (String.ValidString hubUrl) ->
                        let connection =
                            SignalR.connect<Sync.Request, Sync.Request, obj, Sync.Response, Sync.Response>
                                (fun hub ->
                                    hub
                                        .withUrl($"{hubUrl}{Sync.endpoint}")
                                        //                    .useMessagePack()
                                        .withAutomaticReconnect(
                                            {
                                                nextRetryDelayInMilliseconds =
                                                    fun _context ->
                                                        Dom
                                                            .Logger
                                                            .getLogger()
                                                            .Debug (fun () ->
                                                                "Selectors.Hub.hubConnection. SignalR.connect(). withAutomaticReconnect")

                                                        Some timeout
                                            }
                                        )
                                        .onReconnecting(fun ex ->
                                            Dom
                                                .Logger
                                                .getLogger()
                                                .Debug (fun () ->
                                                    $"Selectors.Hub.hubConnection. SignalR.connect(). onReconnecting ex={ex}"))
                                        .onReconnected(fun ex ->
                                            Dom
                                                .Logger
                                                .getLogger()
                                                .Debug (fun () ->
                                                    $"Selectors.Hub.hubConnection. SignalR.connect(). onReconnected ex={ex}"))
                                        .onClose(fun ex ->
                                            Dom
                                                .Logger
                                                .getLogger()
                                                .Debug (fun () ->
                                                    $"Selectors.Hub.hubConnection. SignalR.connect(). onClose ex={ex}"))
                                        .configureLogging(LogLevel.Debug)
                                        .onMessage (fun msg ->
                                            match msg with
                                            | Sync.Response.ConnectResult ->
                                                Dom
                                                    .Logger
                                                    .getLogger()
                                                    .Debug (fun () ->
                                                        "Selectors.Hub.hubConnection. Sync.Response.ConnectResult")
                                            | Sync.Response.SetResult result ->
                                                Dom
                                                    .Logger
                                                    .getLogger()
                                                    .Debug (fun () ->
                                                        $"Selectors.Hub.hubConnection. Sync.Response.SetResult result={result}")
                                            | Sync.Response.GetResult value ->
                                                Dom
                                                    .Logger
                                                    .getLogger()
                                                    .Debug (fun () ->
                                                        $"Selectors.Hub.hubConnection. Sync.Response.GetResult value={value}")
                                            | Sync.Response.GetStream (key, value) ->
                                                Dom
                                                    .Logger
                                                    .getLogger()
                                                    .Debug (fun () ->
                                                        $"Selectors.Hub.hubConnection. Sync.Response.GetStream key={key} value={value}")
                                            | Sync.Response.FilterResult keys ->
                                                Dom
                                                    .Logger
                                                    .getLogger()
                                                    .Debug (fun () ->
                                                        $"Selectors.Hub.hubConnection. Sync.Response.FilterResult keys={keys}")
                                            | Sync.Response.FilterStream (key, keys) ->
                                                Dom
                                                    .Logger
                                                    .getLogger()
                                                    .Debug (fun () ->
                                                        $"Selectors.Hub.hubConnection. Sync.Response.FilterStream key={key} keys={keys}")

                                            match msg with
                                            | Sync.Response.FilterStream (key, keys) ->
                                                match hubSubscriptionMap.TryGetValue key with
                                                | true, fn ->
                                                    Dom
                                                        .Logger
                                                        .getLogger()
                                                        .Debug (fun () ->
                                                            $"Selectors.Hub.hubConnection. Selectors.hub onMsg msg={msg}. triggering ")

                                                    fn keys
                                                | _ ->
                                                    Dom
                                                        .Logger
                                                        .getLogger()
                                                        .Debug (fun () ->
                                                            $"Selectors.Hub.hubConnection. onMsg msg={msg}. skipping. not in map ")
                                            | _ ->
                                                Dom
                                                    .Logger
                                                    .getLogger()
                                                    .Debug (fun () ->
                                                        $"Selectors.Hub.hubConnection.  onMsg msg={msg}. skipping. not handled ")))

                        Dom.Logger.Default.Debug
                            (fun () ->
                                $"Selectors.Hub.hubConnection. end. alias={alias} hubUrl={hubUrl}. starting connection...")

                        connection.startNow ()
                        Some connection
                    | _ -> None)



        let rec hub =
            Store.readSelector
                FsStore.root
                (nameof hub)
                (fun getter ->
                    let hubTrigger = Store.value getter Atoms.hubTrigger
                    let hubConnection = Store.value getter hubConnection

                    match hubConnection with
                    | Some hubConnection ->
                        Dom.Logger.Default.Debug
                            (fun () ->
                                $"Selectors.Hub.hub. hubTrigger={hubTrigger} hubConnection.connectionId={hubConnection.connectionId}")

                        Some hubConnection
                    //                        match Store.value getter atomAccessors with
//                        | Some (getter, setter) ->
//                            Some hubConnection
//                        | None -> None
                    | _ -> None)

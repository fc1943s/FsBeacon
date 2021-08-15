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
            .Debug (fun () -> $"atomAccessors.constructor {getDebugInfo ()}")

        let rec valueWrapper =
            Store.selector
                FsStore.root
                (nameof valueWrapper)
                (fun getter ->
                    let result = Store.value getter valueAtom

                    Dom
                        .Logger
                        .getLogger()
                        .Debug (fun () -> $"atomAccessors.valueWrapper.get() result={result} {getDebugInfo ()}")

                    result)
                (fun getter setter newValue ->
                    Dom
                        .Logger
                        .getLogger()
                        .Debug (fun () -> $"atomAccessors.valueWrapper.set() newValue={newValue} {getDebugInfo ()}")

                    Store.set setter accessorsAtom (Some (getter, setter))
                    Store.set setter valueAtom newValue)

        valueWrapper.onMount <-
            fun setAtom ->
                Dom
                    .Logger
                    .getLogger()
                    .Debug (fun () -> $"atomAccessors.valueWrapper.onMount() lastValue={lastValue} {getDebugInfo ()}")

                lastValue <- lastValue + 1
                setAtom lastValue

                fun () ->
                    Dom
                        .Logger
                        .getLogger()
                        .Debug (fun () ->
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
                    .Debug (fun () ->
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
                    let isTesting = Store.value getter Atoms.isTesting
                    let gunPeers = Store.value getter gunPeers

                    let gun =
                        if isTesting then
                            Bindings.Gun.gun
                                {
                                    GunProps.peers = None
                                    GunProps.radisk = Some false
                                    GunProps.localStorage = Some false
                                    GunProps.multicast = None
                                }
                        else
                            Bindings.Gun.gun
                                {
                                    GunProps.peers = Some gunPeers
                                    GunProps.radisk = Some true
                                    GunProps.localStorage = Some false
                                    GunProps.multicast = None
                                }

                    printfn $"Gun selector. gunPeers={gunPeers}. gun={gun} returning..."

                    gun)


        let rec gunNamespace =
            Store.readSelector
                FsStore.root
                (nameof gunNamespace)
                (fun getter ->
                    let _gunTrigger = Store.value getter Atoms.gunTrigger
                    let gun = Store.value getter gun
                    let user = gun.user ()

                    printfn
                        $"gunNamespace selector.
                            gunPeers={gunPeers}
                            user().is.alias={user.is
                                             |> Option.map
                                                 (fun x ->
                                                     match x.alias with
                                                     | Some (GunUserAlias.Alias username) -> username
                                                     | _ -> Alias null)}
                            user().is={user.is |> Js.objectKeys}
                            user().__.sea={user.__.sea |> Js.objectKeys}..."

                    user :> Types.IGunNode)

        let getRecursiveNode (gunNode: Types.IGunNode) (nodes: GunNodeSlice list) getter username =
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

                let node = Store.value getter (gunAtomNode (username, parentAtomPath))

                node
                |> Option.map (fun (node: Types.IGunChainReference) -> node.get lastNode)

        let rec gunAtomNode =
            Store.readSelectorFamily
                FsStore.root
                (nameof gunAtomNode)
                (fun (username, AtomPath atomPath) getter ->
                    let gun = Store.value getter gun
                    let gunKeys = Store.value getter Atoms.gunKeys

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

                    let user = gun.user ()

                    let getNode () =
                        getRecursiveNode user nodes getter username

                    match user.is with
                    | Some {
                               alias = Some (GunUserAlias.GunKeys gunKeys')
                           } when gunKeys' = gunKeys ->

                        getNode ()
                    | Some {
                               alias = Some (GunUserAlias.Alias (Alias alias))
                           } when alias = (username |> Username.ValueOrDefault) -> getNode ()
                    | _ ->
                        Dom.Logger.Default.Debug
                            (fun () ->
                                $"gunAtomNode. Invalid username.
                                          username={username}
                                          atomPath={atomPath}
                                          user.is={JS.JSON.stringify user.is}")

                        None)



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
                    let username = Store.value getter Atoms.username

                    printfn $"hub connection selector. username={username} hubUrl={hubUrl}"

                    match username, hubUrl with
                    | Some (Username (String.ValidString _)), Some (String.ValidString hubUrl) ->
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
                                                                "SignalR.connect(). withAutomaticReconnect")

                                                        Some timeout
                                            }
                                        )
                                        .onReconnecting(fun ex ->
                                            Dom
                                                .Logger
                                                .getLogger()
                                                .Debug (fun () -> $"SignalR.connect(). onReconnecting ex={ex}"))
                                        .onReconnected(fun ex ->
                                            Dom
                                                .Logger
                                                .getLogger()
                                                .Debug (fun () -> $"SignalR.connect(). onReconnected ex={ex}"))
                                        .onClose(fun ex ->
                                            Dom
                                                .Logger
                                                .getLogger()
                                                .Debug (fun () -> $"SignalR.connect(). onClose ex={ex}"))
                                        .configureLogging(LogLevel.Debug)
                                        .onMessage (fun msg ->
                                            match msg with
                                            | Sync.Response.ConnectResult ->
                                                Dom
                                                    .Logger
                                                    .getLogger()
                                                    .Debug (fun () -> "Sync.Response.ConnectResult")
                                            | Sync.Response.SetResult result ->
                                                Dom
                                                    .Logger
                                                    .getLogger()
                                                    .Debug (fun () -> $"Sync.Response.SetResult result={result}")
                                            | Sync.Response.GetResult value ->
                                                Dom
                                                    .Logger
                                                    .getLogger()
                                                    .Debug (fun () -> $"Sync.Response.GetResult value={value}")
                                            | Sync.Response.GetStream (key, value) ->
                                                Dom
                                                    .Logger
                                                    .getLogger()
                                                    .Debug (fun () ->
                                                        $"Sync.Response.GetStream key={key} value={value}")
                                            | Sync.Response.FilterResult keys ->
                                                Dom
                                                    .Logger
                                                    .getLogger()
                                                    .Debug (fun () -> $"Sync.Response.FilterResult keys={keys}")
                                            | Sync.Response.FilterStream (key, keys) ->
                                                Dom
                                                    .Logger
                                                    .getLogger()
                                                    .Debug (fun () ->
                                                        $"Sync.Response.FilterStream key={key} keys={keys}")

                                            match msg with
                                            | Sync.Response.FilterStream (key, keys) ->
                                                match hubSubscriptionMap.TryGetValue key with
                                                | true, fn ->
                                                    Dom
                                                        .Logger
                                                        .getLogger()
                                                        .Debug (fun () -> $"Selectors.hub onMsg msg={msg}. triggering ")

                                                    fn keys
                                                | _ ->
                                                    Dom
                                                        .Logger
                                                        .getLogger()
                                                        .Debug (fun () ->
                                                            $"Selectors.hub onMsg msg={msg}. skipping. not in map ")
                                            | _ ->
                                                Dom
                                                    .Logger
                                                    .getLogger()
                                                    .Debug (fun () ->
                                                        $"Selectors.hub onMsg msg={msg}. skipping. not handled ")))

                        printfn $"hub connection selector. username={username} hubUrl={hubUrl}. starting connection..."
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
                        printfn
                            $"hub selector. hubTrigger={hubTrigger} hubConnection.connectionId={hubConnection.connectionId}"

                        Some hubConnection
                    //                        match Store.value getter atomAccessors with
//                        | Some (getter, setter) ->
//                            Some hubConnection
//                        | None -> None
                    | _ -> None)

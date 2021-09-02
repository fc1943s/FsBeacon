namespace FsStore

open System
open System.Collections.Generic
open Fable.Core.JsInterop
open Fable.Core
open FsCore.BaseModel
open FsJs
open FsStore.Bindings
open FsStore.Bindings.Batcher
open FsStore.Bindings.Jotai
open FsStore.Model
open FsCore
open FsStore.State

#nowarn "40"


module Engine =
    type UpdateFn<'State, 'Command, 'Event> =
        Getter<obj> -> Setter<obj> -> 'State -> 'Command -> JS.Promise<'State * Message<'Command, 'Event> list>

    let inline getDebugInfo () = ""

    let inline consumeCommands (updateFn: UpdateFn<_, _, _>) state getter setter commands =
        promise {
            //            let logger = Atom.get getter Selectors.logger

            let rec loop state commands processedMessages =
                promise {
                    match commands with
                    | command :: commands ->
                        let! state, processedMessages' = updateFn getter setter state command

                        let commands', events =
                            processedMessages'
                            |> List.partition
                                (function
                                | Message.Command _ -> true
                                | _ -> false)

                        let newCommands =
                            commands'
                            |> List.choose
                                (function
                                | Message.Command command -> Some command
                                | _ -> None)

                        return! loop state (newCommands @ commands) (processedMessages @ events)
                    | [] -> return state, processedMessages
                }

            let! newState, processedMessages = loop state commands []


            let getDebugInfo () =
                $"commands={commands} newState={newState} processedMessages={processedMessages} {getDebugInfo ()}"

            let addTimestamp fn getDebugInfo =
                Profiling.addTimestamp
                    (fun () -> $"{nameof FsStore} | Engine.consumeCommands {fn ()} | {getDebugInfo ()}")

            addTimestamp (fun () -> "[ ](_1)") getDebugInfo

            return
                newState,
                processedMessages
                |> List.choose
                    (function
                    | Message.Event event -> Some event
                    | _ -> None)
        }


    let mutable lastStore: (Getter<obj> * Setter<obj>) option = None

    type MountFn<'A> = Getter<obj> -> Setter<obj> -> ('A -> unit) -> JS.Promise<unit>
    type UnmountFn = Getter<obj> -> Setter<obj> -> unit
    type StateFn<'S> = Getter<obj> -> 'S option
    type StateMountFn<'A, 'S> = Getter<obj> -> Setter<obj> -> 'S -> ('A -> unit) -> JS.Promise<unit>
    type StateUnmountFn<'S> = Getter<obj> -> Setter<obj> -> 'S -> unit

    let inline wrapAtomWithState<'A, 'S when 'A: equality>
        (stateFn: StateFn<'S>)
        (mount: StateMountFn<'A, 'S>)
        (unmount: StateUnmountFn<'S>)
        (atom: AtomConfig<'A>)
        =
        let storeAtomPath =
            if Atom.isRegistered (AtomReference.Atom atom) then
                Some (Atom.query (AtomReference.Atom atom))
            else
                None

        let mutable lastState: 'S option = None

        let mutable mounted = false

        let getDebugInfo () =
            $" | atom={atom} mounted={mounted} storeAtomPath={storeAtomPath |> Option.map StoreAtomPath.AtomPath} lastState.IsSome={lastState.IsSome}  {getDebugInfo ()}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp
                (fun () -> $"{nameof FsStore} | Engine.wrapAtomWithState {fn ()} | {getDebugInfo ()}")

        addTimestamp (fun () -> "[ constructor ](g1)") getDebugInfo

        let mutable lastSetAtom = None

        let inline getState () =
            match lastStore with
            | Some (getter, setter) ->
                let state =
                    match stateFn getter with
                    | Some state -> Some state
                    | None -> lastState

                lastState <- state

                match state with
                | Some state -> Some (getter, setter, state)
                | None -> None
            | _ -> None

        let inline newMount () =
            promise {
                match getState () with
                | Some (getter, setter, state) ->
                    addTimestamp (fun () -> "[ newMount ](g2) invoking mount") getDebugInfo

                    mounted <- true
                    do! mount getter setter state lastSetAtom.Value
                | None -> addTimestamp (fun () -> "[ newMount ](g3) skipping, no state") getDebugInfo
            }

        let inline newUnmount () =
            if mounted then
                match getState () with
                | Some (getter, setter, state) ->
                    addTimestamp (fun () -> "[ newUnmount ](g4) invoking unmount") getDebugInfo

                    mounted <- false
                    lastState <- None
                    unmount getter setter state
                | None ->
                    addTimestamp
                        (fun () -> "[ newUnmount ](g5) invoking unmount skipping, no state. (should unmount here???)")
                        getDebugInfo

        let inline refreshInternalState getter =
            if lastStore.IsNone then lastStore <- Atom.get getter Selectors.store

            let _logger = Atom.get getter Selectors.logger

            let newState = stateFn getter

            let getDebugInfo () =
                $"newState.IsSome={newState.IsSome} lastSetAtom.IsSome={lastSetAtom.IsSome} {getDebugInfo ()}"

            if lastSetAtom.IsNone then
                addTimestamp
                    (fun () -> "[ refreshInternalState ](g5) skipping mount/unmount. lastSetAtom not found")
                    getDebugInfo
            else
                match newState with
                | Some _ ->
                    addTimestamp (fun () -> "[ refreshInternalState ](g6) invoking newMount") getDebugInfo
                    lastState <- newState
                    newMount () |> Promise.start
                | None ->
                    addTimestamp (fun () -> "[ refreshInternalState ](g7) invoking newUnmount") getDebugInfo
                    newUnmount ()

        let wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    refreshInternalState getter

                    let result = Atom.get getter atom
                    let getDebugInfo () = $"result={result}  {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ wrapper.get() ](g8)") getDebugInfo

                    result)
                (fun _getter setter newValue ->
                    //                    refreshInternalState getter

                    let getDebugInfo () =
                        $"newValue={newValue}  {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ wrapper.set() ](g9)") getDebugInfo
                    Atom.set setter atom newValue)
            |> Atom.addSubscription
                false
                (fun setAtom ->
                    addTimestamp (fun () -> "[ addSubscription mount ](g10) invoking newMount") getDebugInfo
                    lastSetAtom <- Some setAtom
                    newMount ())
                (fun () ->
                    addTimestamp (fun () -> "[ addSubscription unmount ](g11) invoking newUnmount") getDebugInfo
                    newUnmount ())

        match storeAtomPath with
        | Some storeAtomPath -> wrapper |> Atom.register storeAtomPath
        | None -> wrapper


    let inline wrapAtom<'A when 'A: equality> (mount: MountFn<'A>) (unmount: UnmountFn) (atom: AtomConfig<'A>) =
        wrapAtomWithState
            (fun _ -> Some ())
            (fun getter setter _ -> mount getter setter)
            (fun getter setter _ -> unmount getter setter)
            atom

    let inline wrapAtomWithInterval defaultValue interval atom =
        let mutable intervalHandle = -1
        let mutable lastValue = None

        let getDebugInfo () =
            $"interval={interval} defaultValue={defaultValue} lastValue={lastValue} timeout={intervalHandle}  {getDebugInfo ()}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp
                (fun () -> $"{nameof FsStore} | Engine.wrapAtomWithInterval {fn ()} | {getDebugInfo ()}")

        let cache = Atom.Primitives.atom defaultValue

        let wrapper =
            atom
            |> wrapAtom
                (fun getter setter _setAtom ->
                    promise {
                        addTimestamp (fun () -> "[ wrapper.mount() ](h1)") getDebugInfo

                        let inline fn () =
                            addTimestamp (fun () -> "[ wrapper.mount.fn() ](h2) interval fn") getDebugInfo

                            if intervalHandle >= 0 then
                                let atomValue = Atom.get getter atom

                                if Some atomValue |> Object.compare lastValue |> not then
                                    let getDebugInfo () =
                                        $"atomValue={atomValue} {getDebugInfo ()}"

                                    addTimestamp
                                        (fun () -> "[ wrapper.mount.fn() ](h3) interval fn. triggering new value")
                                        getDebugInfo

                                    Atom.set setter cache atomValue
                                    lastValue <- Some atomValue

                        if intervalHandle = -1 then fn ()
                        intervalHandle <- JS.setInterval fn interval
                    })
                (fun _getter _setter ->
                    //                let logger = Logger.State.lastLogger
                    addTimestamp (fun () -> "[ wrapper.unmount() ](h4) ") getDebugInfo

                    if intervalHandle >= 0 then JS.clearTimeout intervalHandle
                    intervalHandle <- -1)

        wrapper?init <- defaultValue

        wrapper


    [<Erase>]
    type GroupRef = GroupRef of IComparable

    [<Erase>]
    type AtomValueRef = AtomValueRef of IComparable

    [<Erase>]
    type KeyRef = KeyRef of IComparable

    let typeMetadataMap =
        Dictionary<DataType * Type, {| DefaultValue: AtomValueRef
                                       Decode: Gun.GunKeys
                                           -> Gun.EncryptedSignedValue
                                           -> JS.Promise<(TicksGuid * AtomValueRef) option>
                                       Encode: Gun.GunKeys
                                           -> TicksGuid * AtomValueRef
                                           -> JS.Promise<Gun.EncryptedSignedValue>
                                       OnFormat: Gun.AtomKeyFragment [] -> KeyRef option |}>
            ()

    let collectionTypeMap = Dictionary<StoreRoot * Collection, Type> ()


    let inline batchPutFromUi atomType (gunAtomNode, privateKeys, ticks, newValue: AtomValueRef, onPut) =
        let getDebugInfo () =
            $"atomType={atomType} ticks={ticks} newValue={newValue} {getDebugInfo ()}"

        let typeMetadata = typeMetadataMap.[atomType]

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.batchPutFromUi {fn ()} | {getDebugInfo ()}")

        batch (
            BatchType.Set (
                ticks,
                (fun ticks ->
                    promise {
                        let! newValueJson =
                            promise {
                                if newValue |> Js.ofNonEmptyObj |> Option.isNone then
                                    return null
                                else
                                    let! (Gun.EncryptedSignedValue encrypted) =
                                        typeMetadata.Encode privateKeys (ticks, newValue)

                                    return encrypted
                            }

                        let! putResult =
                            Gun.put
                                gunAtomNode
                                (Gun.GunValue.EncryptedSignedValue (Gun.EncryptedSignedValue newValueJson))

                        let getDebugInfo () =
                            $"putResult={putResult} {getDebugInfo ()}"

                        addTimestamp (fun () -> "[ batchSetFn ](i1)") getDebugInfo
                        if putResult then onPut ()
                    })
            )
        )


    let memoryAdapterOptions = Some Atom.AdapterOptions.Memory

    let inline getAdapterOptions getter adapterType =
        match adapterType with
        | Atom.AdapterType.Gun -> Atom.get getter Selectors.Gun.adapterOptions
        | Atom.AdapterType.Hub -> Atom.get getter Selectors.Hub.adapterOptions
        | Atom.AdapterType.Memory -> memoryAdapterOptions

    type FromUi =
        | FromUi
        | NotFromUi

    [<RequireQualifiedAccess>]
    type KeyOperation =
        | Add
        | Remove

    let collectionKeysFamily =
        Atom.atomFamilyAtom
            (fun (_alias: Gun.Alias option, _storeRoot: StoreRoot, _collection: Collection) ->
                Map.empty: Map<KeyRef, KeyOperation>)


    [<RequireQualifiedAccess>]
    type BatchKind =
        | Replace
        | Union

    type Transaction = Transaction of fromUi: FromUi * ticks: TicksGuid * value: AtomValueRef

    let inline getAdapterSubscription atomType adapterType =

        let typeMetadata = typeMetadataMap.[atomType]

        let debouncedBatchPutFromUi = Js.debounce (batchPutFromUi atomType) 0

        let getDebugInfo () =
            $"atomType={atomType} adapterType={adapterType} {getDebugInfo ()}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp
                (fun () -> $"{nameof FsStore} | Engine.getAdapterSubscription {fn ()} | {getDebugInfo ()}")

        let mount, unmount =
            match adapterType with
            | Atom.AdapterType.Gun ->
                (fun storeAtomPath getter setter adapterOptions (adapterSetAtom: Transaction -> unit) ->
                    let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

                    let getDebugInfo () =
                        $"atomPath={atomPath}  {getDebugInfo ()}"

                    match adapterOptions with
                    | Atom.AdapterOptions.Gun (_peers, alias) ->
                        let privateKeys = Atom.get getter Selectors.Gun.privateKeys
                        let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (Some alias, atomPath))

                        let getDebugInfo () =
                            $" adapterOptions={adapterOptions} {getDebugInfo ()}"

                        addTimestamp (fun () -> "[ ====> mountFn ](j1) gun") getDebugInfo

                        match gunAtomNode, privateKeys with
                        | Some gunAtomNode, Some privateKeys ->
                            let subscriptionTicks = Guid.newTicksGuid ()

                            let setAdapterValues =
                                match storeAtomPath with
                                | IndexedAtomPath (_, _, _, name)
                                | RootAtomPath (_, name) ->
                                    addTimestamp
                                        (fun () -> $"[ |--| mount ] invoking indexed subscribe. name={name} ")
                                        getDebugInfo

                                    let debouncedAdapterSetAtom =
                                        Js.debounce
                                            (fun value ->
                                                let getDebugInfo () = $"value={value} {getDebugInfo ()}"

                                                addTimestamp
                                                    (fun () ->
                                                        "[ ********> mountFn / debouncedAdapterSetAtom ](j3) gun. debounced on subscribe data")
                                                    getDebugInfo

                                                adapterSetAtom value)
                                            0


                                    Gun.batchSubscribe
                                        gunAtomNode
                                        subscriptionTicks
                                        (fun (subscriptionTicks, gunValue, key) ->
                                            promise {
                                                let getDebugInfo () =
                                                    $"gunValue={gunValue} key={key} {getDebugInfo ()}"

                                                try
                                                    addTimestamp
                                                        (fun () ->
                                                            "[ ||==> Gun.batchSubscribe.on() ](j4-1(1)) will decode. inside gun.on() ")
                                                        getDebugInfo

                                                    let! newValue = typeMetadata.Decode privateKeys gunValue

                                                    let getDebugInfo () =
                                                        $"newValue={newValue} {getDebugInfo ()}"

                                                    addTimestamp
                                                        (fun () ->
                                                            "[ ||==> Gun.batchSubscribe.on() ](j4-1(2)) invoking debouncedSetAtom. inside gun.on() ")
                                                        getDebugInfo

                                                    debouncedAdapterSetAtom (
                                                        newValue
                                                        |> Option.map
                                                            (fun (ticks, value) ->
                                                                Transaction (NotFromUi, ticks, value))
                                                        |> Option.defaultValue (unbox null)
                                                    )
                                                with
                                                | ex ->
                                                    Logger.logError
                                                        (fun () ->
                                                            $"Engine.getAtomAdapter. gun subscribe data error. ex={ex.Message} subscriptionTicks={subscriptionTicks} {getDebugInfo ()}")

                                                    Logger.consoleError [| ex |]
                                            })

                                    let inline setAdapterValue (Transaction (fromUi, lastTicks, lastValue)) =
                                        let getDebugInfo () =
                                            $"fromUi={fromUi} lastTicks={lastTicks} lastValue={lastValue} {getDebugInfo ()}"

                                        addTimestamp
                                            (fun () ->
                                                "[ ||==> setAdapterValue ](j4-2) invoking debouncedBatchPutFromUi  ")
                                            getDebugInfo

                                        if fromUi = FromUi then
                                            debouncedBatchPutFromUi (
                                                gunAtomNode,
                                                privateKeys,
                                                lastTicks,
                                                lastValue,
                                                (fun () ->
                                                    addTimestamp
                                                        (fun () ->
                                                            "[ ||==> setAdapterValue ](j4) invoking debouncedSetAtom. gun inside setAtom passed to debouncedBatchPutFromUi  ")
                                                        getDebugInfo

                                                    debouncedAdapterSetAtom (
                                                        Transaction (NotFromUi, lastTicks, lastValue)
                                                    ))
                                            )

                                    Some setAdapterValue
                                | CollectionAtomPath (storeRoot, collection) ->
                                    addTimestamp
                                        (fun () -> "[ |--| mount ] invoking collection subscribe  ")
                                        getDebugInfo

                                    let mutable lastValue: Set<Gun.AtomKeyFragment> option = None

                                    let inline batchKeys
                                        (setAtom: Gun.AtomKeyFragment [] -> JS.Promise<unit>)
                                        data
                                        kind
                                        =
                                        Gun.batchKeys
                                            atomType
                                            setAtom
                                            data
                                            (fun itemsArray ->
                                                let newSet = itemsArray |> Seq.collect snd |> Set.ofSeq

                                                let merge =
                                                    match kind with
                                                    | BatchKind.Replace -> newSet
                                                    | BatchKind.Union ->
                                                        lastValue
                                                        |> Option.defaultValue Set.empty
                                                        |> Set.union newSet

                                                lastValue <- Some merge
                                                let items = merge |> Set.toArray


                                                //                    let newItems =
                                                //                        itemsArray
                                                //                        |> Seq.collect snd
                                                //                        |> Seq.filter (lastSet.Contains >> not)
                                                //                        |> Seq.toArray
                                                //
                                                //                    let items =
                                                //                        itemsArray
                                                //                        |> Array.collect snd
                                                //                        |> Array.append newItems
                                                //                        |> Array.distinct
                                                //
                                                //                    lastValue <- Some (newItems |> Set.ofArray |> Set.union lastSet)
                                                let getDebugInfo () =
                                                    $"lastValue={lastValue} itemsArray={itemsArray} items={items} {getDebugInfo ()}"

                                                addTimestamp (fun () -> "[ batchKeys ](y2) ") getDebugInfo

                                                items)

                                    //                                    let batchKeysAtom (fromUi: FromUi, ticks, value) kind =
                                    //                                        batchKeys adapterSetAtom (fromUi, ticks, value) kind

                                    let inline batchKeysAtom (fromUi, ticks, value) kind =
                                        batchKeys
                                            (fun keys ->
                                                promise {
                                                    keys
                                                    |> Array.map Array.singleton
                                                    |> Array.map typeMetadata.OnFormat
                                                    |> Array.iteri
                                                        (fun i formattedKey ->
                                                            let getDebugInfo () =
                                                                $"fromUi={fromUi} ticks={ticks} subscriptionTicks={subscriptionTicks} i={i} keys={keys} formattedKey={formattedKey} value={value} {getDebugInfo ()}"

                                                            match formattedKey with
                                                            | Some formattedKey ->
                                                                addTimestamp
                                                                    (fun () ->
                                                                        "[ batchKeysAtom ](y2) invoking keys adapterSetAtom (skipping 'adapterSetAtom (Transaction (fromUi, ticks, atomValueRef))' call) ")
                                                                    getDebugInfo

                                                                Atom.change
                                                                    setter
                                                                    (collectionKeysFamily (
                                                                        Some alias,
                                                                        storeRoot,
                                                                        collection
                                                                    ))
                                                                    (fun oldValue ->
                                                                        oldValue
                                                                        |> Map.add formattedKey KeyOperation.Add)

                                                            //                                                                adapterSetAtom (Transaction (fromUi, ticks, atomValueRef))
                                                            | None ->
                                                                addTimestamp
                                                                    (fun () ->
                                                                        "[ batchKeysAtom ](y2) batchKeysAtom skipped. invalid onformat ")
                                                                    getDebugInfo)
                                                })
                                            (ticks, value |> Array.singleton)
                                            kind


                                    Gun.batchSubscribe
                                        (gunAtomNode.map ())
                                        subscriptionTicks
                                        (fun (subscriptionTicks, gunValue, rawKey) ->
                                            promise {
                                                let getDebugInfo () =
                                                    $"rawKey={rawKey} gunValue={gunValue} subscriptionTicks={subscriptionTicks} {getDebugInfo ()}"

                                                addTimestamp
                                                    (fun () ->
                                                        "[ ||==>X Gun.batchSubscribe.on() ](j4-1) inside gun.map().on() ")
                                                    getDebugInfo

                                                match gunValue |> Option.ofObjUnbox with
                                                | Some _ ->
                                                    batchKeysAtom
                                                        (NotFromUi, Guid.newTicksGuid (), rawKey)
                                                        BatchKind.Union
                                                | _ -> eprintfn $"invalid gun.map().on() {getDebugInfo ()}"
                                            })

                                    let inline setAdapterValue
                                        (Transaction (fromUi, lastTicks, AtomValueRef lastValue))
                                        =
                                        let getDebugInfo () =
                                            $"fromUi={fromUi} lastTicks={lastTicks} lastValue={lastValue} {getDebugInfo ()}"

                                        batchKeysAtom
                                            (NotFromUi, Guid.newTicksGuid (), Gun.AtomKeyFragment (string lastValue))
                                            BatchKind.Union

                                        addTimestamp
                                            (fun () ->
                                                "[ ||==> collection setAdapterValue ](j4-2) calling batchKeysAtom  ")
                                            getDebugInfo

                                    Some setAdapterValue


                            addTimestamp (fun () -> "[ ||||||||| mountFn ](j2) gun. will batch subscribe") getDebugInfo

                            match setAdapterValues with
                            | Some setAdapterValues ->
                                let subscriptionId = SubscriptionId subscriptionTicks
                                Some (subscriptionId, setAdapterValues)
                            | None -> None

                        | _ -> failwith $"invalid gun atom node {getDebugInfo ()}"
                    | _ -> None),
                (fun storeAtomPath getter (_setter: Setter<obj>) adapterOptions ->
                    let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

                    let getDebugInfo () =
                        $"atomPath={atomPath} {getDebugInfo ()}"

                    match adapterOptions with
                    | Atom.AdapterOptions.Gun (_peers, alias) ->
                        let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (Some alias, atomPath))

                        match gunAtomNode with
                        | Some _gunAtomNode ->
                            // TODO: delayed unsub (free after 10 seconds)
//                            match storeAtomPath with
//                            | IndexedAtomPath _
//                            | RootAtomPath _ -> gunAtomNode.off () |> ignore
//                            | CollectionAtomPath _ -> gunAtomNode.map().off () |> ignore
                            ()
                        | _ -> ()

                        let getDebugInfo () =
                            $"adapterOptions={adapterOptions} {getDebugInfo ()}"

                        addTimestamp
                            (fun () -> "[ <==== unmountFn ](j5) gun unmount (# skipped, gun bug? ) ")
                            getDebugInfo

                    | _ -> ())
            | Atom.AdapterType.Hub ->
                (fun _storeAtomPath _getter _setter adapterOptions _setValue ->
                    match adapterOptions with
                    | Atom.AdapterOptions.Hub (_alias, _hubUrl) ->
                        //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                        Profiling.addTimestamp (fun () -> $"+09B ====> getAtomAdapter hub mount  {getDebugInfo ()}  ")

                        let subscriptionTicks = Guid.newTicksGuid ()
                        let subscriptionId = SubscriptionId subscriptionTicks

                        Some (
                            subscriptionId,
                            (fun (Transaction (_fromUi, _lastTicks, _lastValue)) ->
                                Profiling.addTimestamp
                                    (fun () -> $"+09-1B ====> getAtomAdapter hub setAdapterValue  {getDebugInfo ()}  "))
                        )
                    | _ -> None),
                (fun _storeAtomPath _getter _setter adapterOptions ->
                    match adapterOptions with
                    | Atom.AdapterOptions.Hub (_alias, _hubUrl) ->
                        //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                        Profiling.addTimestamp (fun () -> $"+08B <==== getAtomAdapter hub unmount  {getDebugInfo ()}  ")
                    | _ -> ()

                    )
            | Atom.AdapterType.Memory ->
                (fun _storeAtomPath _getter _setter adapterOptions _setValue ->
                    match adapterOptions with
                    | Atom.AdapterOptions.Memory ->
                        //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))

                        let getDebugInfo () =
                            $"adapterOptions={adapterOptions} {getDebugInfo ()}"

                        addTimestamp (fun () -> "[ ====> mountFn ](j6) memory mount ") getDebugInfo

                        let subscriptionTicks = Guid.newTicksGuid ()
                        let subscriptionId = SubscriptionId subscriptionTicks

                        Some (
                            subscriptionId,
                            (fun (Transaction (_fromUi, _lastTicks, _lastValue)) ->
                                let getDebugInfo () =
                                    $"_lastTicks={_lastTicks} _lastValue={_lastValue} {getDebugInfo ()}"

                                addTimestamp
                                    (fun () -> "[ ¨¨ setAdapterValue ](j8) memory inside debouncedPutFromUi setAtom ")
                                    getDebugInfo)
                        )
                    | _ -> None),
                (fun _storeAtomPath _getter _setter adapterOptions ->
                    match adapterOptions with
                    | Atom.AdapterOptions.Memory ->
                        //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))

                        let getDebugInfo () =
                            $"adapterOptions={adapterOptions} {getDebugInfo ()}"

                        addTimestamp (fun () -> "[ <==== unmountFn ](j7) memory unmount ") getDebugInfo
                    | _ -> ())

        mount, unmount



    type AtomId = AtomId of adapterType: Atom.AdapterType * alias: Gun.Alias * storeAtomPath: StoreAtomPath

    let gunSubscriptionMap = Dictionary<AtomId, (SubscriptionId * (Transaction -> unit)) option> ()
    //    let collectionSubscriptionMap = Dictionary<StoreRoot * Collection, unit -> unit> ()


    let inline createAtomWithAdapter
        (AtomId (adapterType, alias, storeAtomPath) as atomId)
        (mount: Getter<obj>
                    -> (AtomConfig<obj> -> obj -> unit)
                    -> Atom.AdapterOptions
                    -> (Transaction -> unit)
                    -> (SubscriptionId * (Transaction -> unit)) option)
        (unmount: Getter<obj> -> (AtomConfig<obj> -> obj -> unit) -> Atom.AdapterOptions -> unit)
        : AtomConfig<Transaction option> =
        let atom = Atom.create (AtomType.Atom None)

        let mutable setAdapterValue: (Transaction -> unit) option = None

        let getDebugInfo () =
            $"storeAtomPath={storeAtomPath} alias={alias} adapterType={adapterType} atom={atom} setAdapterValue.IsSome={setAdapterValue.IsSome}  {getDebugInfo ()}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp
                (fun () -> $"{nameof FsStore} | Engine.wrapAtomWithAdapter {fn ()} | {getDebugInfo ()}")

        addTimestamp (fun () -> "[ constructor ](f1)") getDebugInfo

        Atom.Primitives.selector
            (fun getter ->
                let result = Atom.get getter atom
                let getDebugInfo () = $"result={result} {getDebugInfo ()}"
                addTimestamp (fun () -> "[ wrapper.get() ](f2)") getDebugInfo
                result)
            (fun _ setter newValue ->
                match newValue with
                | Some (Transaction (newFromUi, newTicks, newValue)) ->
                    Atom.change
                        setter
                        atom
                        (fun oldValue ->
                            let getDebugInfo () =
                                $"oldValue={oldValue} newTicks={newTicks} newValue={newValue} newFromUi={newFromUi} {getDebugInfo ()}"

                            match setAdapterValue with
                            | Some setAdapterValue when
                                (match oldValue with
                                 | None when newFromUi = FromUi -> true
                                 | None -> false
                                 | Some (Transaction (_, _, oldValue)) when oldValue |> Object.compare newValue |> not ->
                                     true
                                 | _ -> false)
                                ->
                                addTimestamp
                                    (fun () -> "[ (^^^^2) wrapper.set() ](f3) triggering new adapter value")
                                    getDebugInfo

                                // gunPut
                                setAdapterValue (Transaction (newFromUi, newTicks, newValue))
                            | _ ->
                                addTimestamp
                                    (fun () -> "[ wrapper.set() ](f3-1) skipping new adapter assign")
                                    getDebugInfo

                            Some (Transaction (newFromUi, newTicks, newValue)))
                | None -> failwith $"invalid newValue {getDebugInfo ()}")
        |> wrapAtomWithState
            (fun getter ->
                let adapterOptions = getAdapterOptions getter adapterType

                let getDebugInfo () =
                    $"adapterOptions={Json.encodeWithNull adapterOptions} {getDebugInfo ()}"

                addTimestamp (fun () -> "[ state.read() ](f5)") getDebugInfo

                match adapterOptions with
                | Some adapterOptions -> Some adapterOptions
                | None -> None)
            (fun getter setter adapterOptions setAtom ->
                promise {
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))

                    let getDebugInfo () =
                        $"adapterOptions={Json.encodeWithNull adapterOptions} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ @@> mount ](f6)") getDebugInfo

                    if setAdapterValue.IsNone then
                        let mountResult =
                            mount
                                getter
                                setter
                                adapterOptions
                                (fun (Transaction (fromUi, ticksGuid, value)) ->

                                    let getDebugInfo () =
                                        $"fromUi={fromUi} ticksGuid={ticksGuid} value={value} {getDebugInfo ()}"

                                    addTimestamp
                                        (fun () -> "[ setAdapterValue / setAtom ](f6+1) after debounced put?")
                                        getDebugInfo

                                    if setAdapterValue.IsNone then
                                        addTimestamp
                                            (fun () -> "[ setAdapterValue / setAtom ](f6+2) skipping assign from adapter. unmounted ")
                                            getDebugInfo
                                    else
                                        setAtom (Some (Transaction (fromUi, ticksGuid, value))))

                        match mountResult with
                        | Some (subscriptionId, setAdapterValues) ->
                            setAdapterValue <- Some setAdapterValues
                            gunSubscriptionMap.Add (atomId, Some (subscriptionId, setAdapterValues))
                        | _ ->
                            gunSubscriptionMap.Remove atomId |> ignore
                            setAdapterValue <- None

                    addTimestamp (fun () -> "[ after mount ](f6)") getDebugInfo

                })
            (fun getter setter adapterOptions ->
                let getDebugInfo () =
                    $"adapterOptions={Json.encodeWithNull adapterOptions} {getDebugInfo ()}"

                addTimestamp (fun () -> "[ <@@ unmount ](f7)") getDebugInfo

                if setAdapterValue.IsSome then
                    unmount getter setter adapterOptions
                    gunSubscriptionMap.Remove atomId |> ignore
                    setAdapterValue <- None

                let getDebugInfo () = $" {getDebugInfo ()}"

                addTimestamp (fun () -> "[ after unmount ](f7)") getDebugInfo)

    let adapterAtomMap = Dictionary<AtomId, AtomConfig<Transaction option>> ()

    let inline getAdapterValues atomType getter storeAtomPath =
        let alias = Atom.get getter Selectors.Gun.alias

        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

        let getDebugInfo () =
            $"atomType={atomType} alias={alias} atomPath={atomPath} {getDebugInfo ()}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.getAdapterValues {fn ()} | {getDebugInfo ()}")

        Reflection.unionCases<Atom.AdapterType>
        |> List.choose
            (fun adapterType ->
                match alias, getAdapterOptions getter adapterType with
                | Some alias, Some adapterOptions ->
                    let getDebugInfo () =
                        $"adapterType={adapterType} adapterOptions={adapterOptions}  {getDebugInfo ()}"

                    let atomId = AtomId (adapterType, alias, storeAtomPath)

                    let mount, unmount = getAdapterSubscription atomType adapterType

                    let adapterAtom =
                        match adapterAtomMap.TryGetValue atomId with
                        | true, value ->
                            addTimestamp (fun () -> "[ getAdapterValues ](a4) returning cached adapter") getDebugInfo

                            value
                        | _ ->
                            let mount, unmount = mount storeAtomPath, unmount storeAtomPath
                            let newAtom = createAtomWithAdapter atomId mount unmount

                            addTimestamp
                                (fun () -> "[ getAdapterValues ](a4) returning newly created adapter")
                                getDebugInfo

                            adapterAtomMap.Add (atomId, newAtom)

                            newAtom

                    let adapterValue = Atom.get getter adapterAtom

                    let getDebugInfo () =
                        $"adapterType={adapterType} adapterAtom={adapterAtom} adapterOptions={adapterOptions} adapterValue={adapterValue} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ getAdapterValues ](a4) returning valid adapter") getDebugInfo
                    Some (adapterType, adapterOptions, adapterAtom, adapterValue)
                | _ -> None)


    let inline subscribeCollection<'TKey, 'A4 when 'TKey: equality and 'A4: equality and 'TKey :> IComparable>
        storeRoot
        collection
        (onFormat: Gun.AtomKeyFragment [] -> 'TKey option)
        //        (_onFormat: string -> 'TKey)
        =
        let collectionAtomType = typeof<'TKey []>
        let atomType = DataType.Key, collectionAtomType

        let storeAtomPath = CollectionAtomPath (storeRoot, collection)

        let getDebugInfo () =
            $"atomType={atomType} atomPath={storeAtomPath |> StoreAtomPath.AtomPath} {getDebugInfo ()}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp
                (fun () -> $"{nameof FsStore} | Engine.subscribeFamilyKey {fn ()} | {getDebugInfo ()}")

        if typeMetadataMap.ContainsKey atomType |> not then
            collectionTypeMap.[(storeRoot, collection)] <- collectionAtomType

            typeMetadataMap.[atomType] <-
                {|
                    DefaultValue = AtomValueRef (([||]: 'TKey []) |> unbox<IComparable>)
                    OnFormat =
                        (fun keys ->
                            let result =
                                keys
                                |> Array.map Array.singleton
                                |> Array.choose onFormat
                                |> function
                                    | [| key |] -> Some (KeyRef key)
                                    | _ -> None

                            let getDebugInfo () =
                                $"keys={keys} result={result} {getDebugInfo ()}"

                            addTimestamp (fun () -> "[ OnFormat ]") getDebugInfo
                            result)
                    Decode = unbox null
                    Encode = unbox null
                |}

        addTimestamp (fun () -> "[ constructor ](z1)") getDebugInfo

        let inline sync _adapterValues =
            match lastStore with
            | Some (getter, _setter) ->
                let adapterValues = getAdapterValues atomType getter storeAtomPath

                let getDebugInfo () =
                    $"adapterValues={adapterValues} {getDebugInfo ()}"

                addTimestamp (fun () -> "[ sync ](z4)") getDebugInfo
            | _ -> ()

        let debouncedSync =
            Js.debounce
                (fun adapterValues ->
                    addTimestamp (fun () -> "[ debouncedSync ](z2)") getDebugInfo
                    sync adapterValues)
                0

        let inline refreshAdapterValues getter =
            let adapterValues = getAdapterValues atomType getter storeAtomPath

            let getDebugInfo () =
                $"adapterValues={adapterValues} {getDebugInfo ()}"

            addTimestamp (fun () -> "[ refreshAdapterValues ](z3)") getDebugInfo

            debouncedSync adapterValues

        Atom.Primitives.readSelector
            (fun getter ->
                refreshAdapterValues getter

                let alias = Atom.get getter Selectors.Gun.alias

                let getDebugInfo () = $"alias={alias} {getDebugInfo ()}"

                let collectionKeys = Atom.get getter (collectionKeysFamily (alias, storeRoot, collection))

                let result =
                    collectionKeys
                    |> Map.filter
                        (fun _ keyOperation ->
                            match keyOperation with
                            | KeyOperation.Add -> true
                            | _ -> false)
                    |> Map.keys
                    |> Seq.toArray

                let getDebugInfo () =
                    $"collectionKeys={collectionKeys} result={Json.encodeWithNull result} {getDebugInfo ()}"

                addTimestamp (fun () -> "[ wrapper.get() ](z3) ") getDebugInfo

                result
                |> Array.map (fun (KeyRef key) -> key |> unbox<'TKey>)

                )
        |> Atom.split

    let inline groupValues groupMap =
        groupMap
        |> Seq.groupBy fst
        |> Seq.map
            (fun (group, values) ->
                let newItem =
                    values
                    |> Seq.sortByDescending fst
                    |> Seq.map snd
                    |> Seq.head

                group, newItem)
        |> Seq.sortByDescending (fun (_, (ticks, _)) -> ticks)
        |> Seq.toList


    let groupMapFamily: (GroupRef * Gun.Alias option * StoreAtomPath -> AtomConfig<(GroupRef * (TicksGuid * AtomValueRef)) list>) =
        Atom.atomFamilyAtom
            (fun (groupRef: GroupRef, alias: Gun.Alias option, storeAtomPath: StoreAtomPath) ->
                let getDebugInfo () =
                    $"groupRef={groupRef} alias={alias} storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath} {getDebugInfo ()}"

                let addTimestamp fn getDebugInfo =
                    Profiling.addTimestamp
                        (fun () -> $"{nameof FsStore} | Engine.groupMapAtom {fn ()} | {getDebugInfo ()}")

                addTimestamp (fun () -> "[ constructor ](e3)") getDebugInfo
                [])


    let userAtomFamily =
        Atom.Primitives.readSelectorFamily
            (fun (groupRef: GroupRef, alias: Gun.Alias option, storeAtomPath: StoreAtomPath) getter ->
                let groupMap = Atom.get getter (groupMapFamily (groupRef, alias, storeAtomPath))

                let getDebugInfo () =
                    $"groupRef={groupRef} alias={alias} storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath} {getDebugInfo ()}"

                let addTimestamp fn getDebugInfo =
                    Profiling.addTimestamp
                        (fun () -> $"{nameof FsStore} | Engine.userAtomFamily {fn ()} | {getDebugInfo ()}")

                addTimestamp (fun () -> "[ readSelectorFamily.read() ](e2)") getDebugInfo

                match groupMap with
                | [] -> []
                | _ -> groupMap |> groupValues)

    let inline createRegisteredAtomWithGroup<'TGroup, 'A6 when 'A6: equality and 'TGroup :> IComparable and 'A6 :> IComparable>
        (storeAtomPath: StoreAtomPath)
        (defaultGroup: 'TGroup, defaultValue: 'A6)
        : AtomConfig<('TGroup * (TicksGuid * 'A6)) list> =

        let getDebugInfo () =
            $"atomPath={storeAtomPath |> StoreAtomPath.AtomPath} defaultGroup={defaultGroup} defaultValue={defaultValue} {getDebugInfo ()}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp
                (fun () -> $"{nameof FsStore} | Engine.createRegisteredAtomWithGroup {fn ()} | {getDebugInfo ()}")

        addTimestamp (fun () -> "[ constructor ](d1)") getDebugInfo

        let wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    let alias = Atom.get getter Selectors.Gun.alias
                    let userAtom = userAtomFamily (GroupRef (unbox defaultGroup), alias, storeAtomPath)
                    let lastSyncValueByType = Atom.get getter userAtom

                    let result =
                        lastSyncValueByType
                        |> Seq.sortByDescending (fun (_, (ticks, _)) -> ticks)
                        |> Seq.toList

                    let filteredResult =
                        result
                        |> List.filter
                            (fun (_, (group', _)) ->
                                result.Length = 1
                                || group' <> (result |> List.head |> snd |> fst))
                    //                        |> unbox<(TicksGuid * ('TGroup * 'A)) list>


                    let finalResult =
                        filteredResult
                        |> List.map
                            (fun (groupRef, (ticks, atomValueRef: AtomValueRef)) ->
                                groupRef |> unbox<'TGroup>, (ticks, atomValueRef |> unbox<'A6>))
                        |> function
                            | [] ->
                                [
                                    defaultGroup, (Guid.Empty, defaultValue)
                                ]
                            | result -> result

                    let getDebugInfo () =
                        $"alias={alias} userAtom={userAtom} result={result} filteredResult={Json.encodeWithNull filteredResult} finalResult={finalResult} {getDebugInfo ()} "

                    addTimestamp (fun () -> "[ wrapper.get() ](d2)") getDebugInfo

                    finalResult)
                (fun getter setter (newValue: ('TGroup * (TicksGuid * 'A6)) list) ->
                    let alias = Atom.get getter Selectors.Gun.alias
                    let userAtom = groupMapFamily (GroupRef defaultGroup, alias, storeAtomPath)

                    let newValue =
                        newValue
                        |> List.map (fun (group, (ticks, value: 'A6)) -> GroupRef group, (ticks, AtomValueRef value))

                    let getDebugInfo () =
                        $"newValue={newValue} alias={alias} userAtom={userAtom} {getDebugInfo ()} "

                    addTimestamp (fun () -> "[ wrapper.set() ](d3)") getDebugInfo

                    Atom.set setter userAtom newValue)
            |> Atom.register storeAtomPath

        wrapper?init <- defaultValue

        wrapper

    let inline sync
        (adapterValues: (Atom.AdapterType * Atom.AdapterOptions * AtomConfig<Transaction option> * Transaction option) list)
        (atom: AtomConfig<(Atom.AdapterType * Transaction) list>)
        : unit =
        match lastStore with
        | Some (getter, setter) ->
            let getDebugInfo () = $"atom={atom} {getDebugInfo ()}"

            let addTimestamp fn getDebugInfo =
                Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.sync {fn ()} | {getDebugInfo ()}")

            let localAdapters = Atom.get getter atom

            let lastAdapterType, Transaction (lastFromUi, lastTicks, lastValue) = localAdapters |> List.head

            let inline setAdapterAtom adapterAtom =
                fun (Transaction (fromUi, ticks, newValue)) ->
                    let getDebugInfo () =
                        $"adapterAtom={adapterAtom} fromUi={fromUi} ticks={ticks} newValue={newValue} {getDebugInfo ()} "

                    addTimestamp (fun () -> "[ (:::::) setAdapterAtom ](c1) ") getDebugInfo

                    Atom.set setter adapterAtom (Some (Transaction (fromUi, ticks, newValue)))

            let values =
                adapterValues
                |> List.map
                    (function
                    | adapterType, _adapterOptions, adapterAtom, Some (Transaction (fromUi, adapterTicks, adapterValue)) ->
                        setAdapterAtom adapterAtom, adapterType, Some fromUi, Some adapterTicks, Some adapterValue
                    | adapterType, _adapterOptions, adapterAtom, None ->
                        setAdapterAtom adapterAtom, adapterType, None, None, None)

            let getDebugInfo () =
                $" lastAdapterType={lastAdapterType} lastFromUi={lastFromUi} lastTicks={lastTicks} lastValue={lastValue} == [ localAdapters={Json.encodeWithNull localAdapters} ] {getDebugInfo ()} "


            let inline setUiAtom (Transaction (fromUi, ticks, newValue)) =
                addTimestamp (fun () -> "[ (:::::) lastAdapter.write() ](c2) invoking Atom.change ") getDebugInfo

                Atom.change
                    setter
                    atom
                    (fun oldValue ->
                        let getDebugInfo () =
                            $"lastAdapterType={lastAdapterType} oldValue={oldValue} ticks={ticks} newValue={newValue} {getDebugInfo ()} "

                        addTimestamp (fun () -> "[ (:::::) lastAdapter.write() ](c2) inside Atom.change ") getDebugInfo

                        (lastAdapterType, (Transaction (fromUi, ticks, newValue)))
                        :: (oldValue
                            |> List.filter (fun (adapterType, _) -> adapterType <> lastAdapterType)))

            let newValues =
                values
                |> List.append [
                    setUiAtom, lastAdapterType, Some lastFromUi, Some lastTicks, Some lastValue
                   ]
                |> List.sortByDescending (fun (_, _, _, ticks, _) -> ticks)

            let valuesfmt =
                Json.encodeWithNull (
                    newValues
                    |> List.map
                        (fun (_setAdapterAtom, adapterType, fromUi, ticksGuid, value) ->
                            adapterType, fromUi, ticksGuid, value)
                )

            let getDebugInfo () =
                $"  {getDebugInfo ()} values={valuesfmt}"

            let _lastAdapterSetAtom, lastAdapterType, lastFromUi, lastTicks, lastValue = newValues.Head
            //                | _ -> ()
//            let lastAdapterSetAtom, lastAdapterType, (lastFromUi, lastTicks, lastValue) = newValues.Head

            addTimestamp (fun () -> "[ sync ](c1-1) init. from debounce. will fan out") getDebugInfo

            newValues
            |> List.skip 1
            |> List.map
                (function
                | setAdapterAtom, adapterType, fromUi, ticks, value when value |> Object.compare lastValue |> not ->
                    promise {
                        let getDebugInfo () =
                            $" adapterFromUi={fromUi} lastFromUi={lastFromUi} adapterType={adapterType} lastAdapterType={lastAdapterType} lastTicks={lastTicks} ticks={ticks} lastValue={lastValue} value={value} {getDebugInfo ()} "

                        match lastFromUi, lastTicks, lastValue with
                        | Some lastFromUi, Some lastTicks, Some lastValue when ticks.IsNone || lastTicks > ticks.Value ->
                            // set adapter value from local atom
                            addTimestamp (fun () -> "[ (%%%%) invalidAdapter.write() ](c4)") getDebugInfo
                            setAdapterAtom (Transaction (lastFromUi, lastTicks, lastValue))
                        | _ ->
                            addTimestamp
                                (fun () -> "[ (%%%%) invalidAdapter.write() ](c3) same ticks. skipping")
                                getDebugInfo
                    }
                | _ -> Promise.lift ())
            |> List.toArray
            |> Promise.all
            |> Promise.ignore
            |> Promise.start
        | _ -> ()

    let inline createRegisteredAtomWithSubscription<'A8 when 'A8: equality and 'A8 :> IComparable>
        storeAtomPath
        (defaultValue: 'A8)
        : AtomConfig<'A8> =
        let atomType = DataType.Data, typeof<'A8>

        if typeMetadataMap.ContainsKey atomType |> not then
            typeMetadataMap.[atomType] <-
                {|
                    DefaultValue = AtomValueRef defaultValue
                    Decode = unbox Gun.userDecode<TicksGuid * 'A8>
                    Encode = unbox Gun.userEncode<TicksGuid * 'A8>
                    OnFormat = unbox null
                |}

        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

        let getDebugInfo () =
            $"atomType={atomType} atomPath={atomPath} defaultValue={defaultValue} {getDebugInfo ()}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp
                (fun () ->
                    $"{nameof FsStore} | Engine.createRegisteredAtomWithSubscription {fn ()} | {getDebugInfo ()}")

        let localAdaptersAtom =
            createRegisteredAtomWithGroup storeAtomPath (Atom.AdapterType.Memory, (NotFromUi, defaultValue))
            |> Atom.map
                (fun value ->
                    let result =
                        value
                        |> List.map
                            (fun (adapterType, (ticksGuid, (fromUi, value: 'A8))) ->
                                adapterType, (Transaction (fromUi, ticksGuid, AtomValueRef value)))

                    let getDebugInfo () =
                        $"value={value} result={result} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ <((A))> wrapper.get() ](a6)") getDebugInfo
                    result)
                (fun (newValue: (Atom.AdapterType * Transaction) list) ->
                    let newValue =
                        newValue
                        |> List.map
                            (fun (adapterType, Transaction (ticksGuid, fromUi, value)) ->
                                adapterType, (fromUi, (ticksGuid, value |> unbox<'A8>)))

                    let getDebugInfo () =
                        $"newValue={newValue} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ <((A))> wrapper.set() ](a6)") getDebugInfo
                    newValue)

        let getDebugInfo () =
            $"localAdaptersAtom={localAdaptersAtom} {getDebugInfo ()}"

        addTimestamp (fun () -> "[ constructor ](a1)") getDebugInfo

        let debouncedSync =
            Js.debounce
                (fun adapterValues ->
                    addTimestamp (fun () -> "[ debouncedSync ](a2)") getDebugInfo
                    sync adapterValues localAdaptersAtom)
                0

        let inline refreshAdapterValues getter =
            let adapterValues = getAdapterValues atomType getter storeAtomPath

            let getDebugInfo () =
                $"adapterValues={adapterValues} {getDebugInfo ()}"

            addTimestamp (fun () -> "[ refreshAdapterValues ](a3)") getDebugInfo
            debouncedSync adapterValues

        let rec wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    refreshAdapterValues getter
                    let localAdapters = Atom.get getter localAdaptersAtom
                    let _adapterType, Transaction (_fromUi, _ticks, value) = localAdapters |> List.head


                    let getDebugInfo () =
                        $"_ticks={_ticks} _adapterType={_adapterType} value={value} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ wrapper.get() ](a5)") getDebugInfo

                    value |> unbox<'A8>)
                (fun getter setter (newValue: 'A8) ->
                    //                    refreshAdapterValues getter

                    let getDebugInfo () =
                        $"newValue={newValue} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ <&¨¨&> wrapper.set() ](a6)") getDebugInfo

                    Atom.change
                        setter
                        localAdaptersAtom
                        (fun localAdapters ->
                            let newItem =
                                Atom.AdapterType.Memory,
                                Transaction (FromUi, Guid.newTicksGuid (), AtomValueRef newValue)

                            let getDebugInfo () =
                                $"localAdapters={Json.encodeWithNull localAdapters} newItem={newItem} {getDebugInfo ()}"

                            addTimestamp (fun () -> "[ <&¨<->¨&> wrapper.set() ](a6)") getDebugInfo

                            localAdapters
                            |> List.filter (fun (adapterType, _) -> adapterType <> Atom.AdapterType.Memory)
                            |> List.append (newItem |> List.singleton))


                    let alias = Atom.get getter Selectors.Gun.alias
                    let collectionPath = storeAtomPath |> StoreAtomPath.CollectionPath
                    let keys = storeAtomPath |> StoreAtomPath.Keys

                    let getDebugInfo () =
                        $"collectionPath={collectionPath} keys={keys} {getDebugInfo ()}"

                    match collectionPath with
                    | Some (storeRoot, collection) ->
                        let collectionAtomType = collectionTypeMap.[(storeRoot, collection)]
                        let typeMetadata = typeMetadataMap.[(DataType.Key, collectionAtomType)]

                        let keys = keys |> Option.bind typeMetadata.OnFormat

                        match keys with
                        | Some keys ->
                            addTimestamp
                                (fun () -> "[ updateKey ](a6-1) saving key. invoking collection Atom.change ")
                                getDebugInfo

                            Atom.change
                                setter
                                (collectionKeysFamily (alias, storeRoot, collection))
                                (fun oldValue -> oldValue |> Map.add keys KeyOperation.Add)
                        | None ->
                            addTimestamp
                                (fun () -> "[ updateKey ](a6-1) skipping key. skipping Atom.change ")
                                getDebugInfo

                    | _ -> addTimestamp (fun () -> "[ updateKey ](a6-2) skipping collection Atom.change ") getDebugInfo)

        wrapper?init <- defaultValue

        wrapper |> Atom.register storeAtomPath

    let inline bindAtom<'A9 when 'A9: equality> atom1 atom2 =
        let mutable lastSetAtom: ('A9 option -> unit) option = None
        let mutable lastValue = None

        let storeAtomPath = Atom.query (AtomReference.Atom atom1)

        let getDebugInfo () =
            $"atom1={atom1} atom2={atom2} atomPath={storeAtomPath |> StoreAtomPath.AtomPath} lastValue={lastValue} {getDebugInfo ()}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.bindAtom {fn ()} | {getDebugInfo ()}")

        addTimestamp (fun () -> "[ constructor ](b1)") getDebugInfo

        let rec wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    match atom1.init, atom2.init with
                    | default1, default2 when default1 <> unbox null && default2 <> unbox null ->
                        match Atom.get getter atom1, Atom.get getter atom2 with
                        | value1, value2 when
                            value1 |> Object.compare default1.Value
                            && (value2 |> Object.compare default2.Value
                                || lastValue.IsNone
                                || (Atom.get getter Selectors.Gun.alias).IsNone)
                            ->
                            let getDebugInfo () =
                                $"value1={value1} value2={value2} {getDebugInfo ()}"

                            addTimestamp (fun () -> "[ wrapper.get() ](b2) choosing value2") getDebugInfo
                            value2
                        | value1, value2 ->
                            let getDebugInfo () =
                                $"value1={value1} value2={value2} {getDebugInfo ()}"

                            match lastSetAtom with
                            | Some lastSetAtom when
                                lastValue.IsNone
                                || lastValue |> Object.compare (Some value1) |> not
                                ->
                                addTimestamp
                                    (fun () -> "[ wrapper.get() ](b3) different. triggering additional")
                                    getDebugInfo

                                lastValue <- Some value1
                                lastSetAtom (Some value1)
                            | _ -> ()

                            addTimestamp (fun () -> "[ wrapper.get() ](b4) choosing value1") getDebugInfo

                            value1
                    | _ -> failwith $"bindAtom. atoms without default value. {getDebugInfo ()}")
                (fun _get setter newValue ->
                    let getDebugInfo () =
                        $"newValue={newValue} {getDebugInfo ()}"

                    if lastValue.IsNone
                       || lastValue |> Object.compare (Some newValue) |> not then
                        lastValue <- Some newValue
                        Atom.set setter atom1 newValue

                        addTimestamp (fun () -> "[ wrapper.set() ](b5) setting atom1 and atom2") getDebugInfo
                    else
                        addTimestamp (fun () -> "[ wrapper.set() ](b6) setting atom2 only") getDebugInfo

                    Atom.set setter atom2 newValue)

        wrapper?init <- atom1.init

        wrapper |> Atom.register storeAtomPath


    let inline createRegisteredAtomWithSubscriptionStorage storeAtomPath (defaultValue: 'A10) =
        //        let storageAtom = Atom.createRegisteredWithStorage storeAtomPath (Guid.Empty, defaultValue)
        let storageAtom = Atom.createRegisteredWithStorage<'A10> storeAtomPath defaultValue
        let syncAtom = createRegisteredAtomWithSubscription storeAtomPath defaultValue
        bindAtom<'A10> syncAtom storageAtom

    let inline getKeysFormatter fn id =
        id
        |> Option.ofObjUnbox
        |> Option.map fn
        |> Option.defaultValue []


    let inline parseGuidKey fn keys =
        match keys |> Array.toList with
        | Gun.AtomKeyFragment guid :: _ ->
            match Guid.TryParse guid with
            | true, guid -> Some (fn guid)
            | _ -> None
        | _ -> None

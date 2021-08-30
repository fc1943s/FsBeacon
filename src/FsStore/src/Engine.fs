namespace FsStore

open System
open System.Collections.Generic
open Fable.Core.JsInterop
open Fable.Core
open FsJs
open FsStore.Bindings
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

        let getState () =
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

        let newMount () =
            promise {
                match getState () with
                | Some (getter, setter, state) ->
                    addTimestamp (fun () -> "[ newMount ](g2) invoking mount") getDebugInfo

                    mounted <- true
                    do! mount getter setter state lastSetAtom.Value
                | None -> addTimestamp (fun () -> "[ newMount ](g3) skipping, no state") getDebugInfo
            }

        let newUnmount () =
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

        let refreshInternalState getter =
            if lastStore.IsNone then lastStore <- Atom.get getter Selectors.store

            let logger = Atom.get getter Selectors.logger
            Logger.State.lastLogger <- logger

            let newState = stateFn getter

            let getDebugInfo () =
                $"newState.IsSome={newState.IsSome} lastSetAtom.IsNone={lastSetAtom.IsNone} {getDebugInfo ()}"

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
                true
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

                        let fn () =
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
    type GroupRef = GroupRef of string

    [<Erase>]
    type AtomValueRef = AtomValueRef of obj

    let typeMetadataMap =
        Dictionary<Type, {| DefaultValue: AtomValueRef
                            Decode: Gun.GunKeys
                                -> Gun.EncryptedSignedValue
                                -> JS.Promise<(TicksGuid * AtomValueRef) option>
                            Encode: Gun.GunKeys -> TicksGuid * AtomValueRef -> JS.Promise<Gun.EncryptedSignedValue> |}>
            ()


    let inline batchPutFromUi atomType gunAtomNode privateKeys ticks (newValue: AtomValueRef) onPut =
        let getDebugInfo () =
            $"atomType={atomType} ticks={ticks} newValue={newValue} {getDebugInfo ()}"

        let typeMetadata = typeMetadataMap.[atomType]

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.batchPutFromUi {fn ()} | {getDebugInfo ()}")

        Batcher.batch (
            Batcher.BatchType.Set (
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
                        ()
                    })
            )
        )


    let memoryAdapterOptions = Some Atom.AdapterOptions.Memory

    let inline getAdapterOptions getter adapterType =
        match adapterType with
        | Atom.AdapterType.Gun -> Atom.get getter Selectors.Gun.adapterOptions
        | Atom.AdapterType.Hub -> Atom.get getter Selectors.Hub.adapterOptions
        | Atom.AdapterType.Memory -> memoryAdapterOptions

    let inline getAdapterSubscription atomType adapterType =

        let typeMetadata = typeMetadataMap.[atomType]

        let debouncedBatchPutFromUi = Js.debounce (batchPutFromUi atomType) 0

        let getDebugInfo () =
            $"adapterType={adapterType} {getDebugInfo ()}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp
                (fun () -> $"{nameof FsStore} | Engine.getAdapterSubscription {fn ()} | {getDebugInfo ()}")

        let mount, unmount =
            match adapterType with
            | Atom.AdapterType.Gun ->
                (fun storeAtomPath getter (_setter: Setter<obj>) adapterOptions adapterSetAtom ->
                    let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

                    let getDebugInfo () =
                        $"atomPath={atomPath}  {getDebugInfo ()}"

                    match adapterOptions with
                    | Atom.AdapterOptions.Gun (_peers, alias) ->
                        let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (Some alias, atomPath))
                        let privateKeys = Atom.get getter Selectors.Gun.privateKeys

                        let getDebugInfo () =
                            $"gunAtomNode={gunAtomNode} privateKeys={privateKeys} adapterOptions={adapterOptions} {getDebugInfo ()}"

                        addTimestamp (fun () -> "[ ====> mountFn ](j1) gun") getDebugInfo

                        match gunAtomNode, privateKeys with
                        | Some gunAtomNode, Some privateKeys ->
                            addTimestamp (fun () -> "[ ||||||||| mountFn ](j2) gun. will batch subscribe") getDebugInfo

                            let debouncedAdapterSetAtom =
                                Js.debounce
                                    (fun value ->
                                        let getDebugInfo () = $"value={value} {getDebugInfo ()}"

                                        addTimestamp
                                            (fun () -> "[ ********> mountFn ](j3) gun. debounced on subscribe data")
                                            getDebugInfo

                                        adapterSetAtom value)
                                    0

                            Gun.batchSubscribe
                                gunAtomNode
                                (Guid.newTicksGuid ())
                                (fun (subscriptionTicks, gunValue) ->
                                    promise {
                                        try
                                            let! newValue = typeMetadata.Decode privateKeys gunValue

                                            addTimestamp
                                                (fun () ->
                                                    "[ ||==> setAdapterValue ](j4-1) invoking debouncedSetAtom. inside gun.on() ")
                                                getDebugInfo

                                            debouncedAdapterSetAtom (
                                                false,
                                                newValue
                                                |> unbox<(TicksGuid * 'A2) option>
                                                |> Option.defaultValue (unbox null)
                                            )
                                        with
                                        | ex ->
                                            Logger.logError
                                                (fun () ->
                                                    $"Engine.getAtomAdapter. gun subscribe data error. ex={ex.Message} gunValue={gunValue} subscriptionTicks={subscriptionTicks} {getDebugInfo ()}")

                                            Logger.consoleError [| ex |]
                                    })

                            let setAdapterValue (fromUi: bool, (lastTicks, lastValue: 'A2)) =
                                let getDebugInfo () =
                                    $"fromUi={fromUi} lastTicks={lastTicks} lastValue={lastValue} {getDebugInfo ()}"

                                addTimestamp
                                    (fun () -> "[ ||==> setAdapterValue ](j4-2) invoking debouncedBatchPutFromUi  ")
                                    getDebugInfo

                                if fromUi then
                                    debouncedBatchPutFromUi
                                        gunAtomNode
                                        privateKeys
                                        lastTicks
                                        (unbox lastValue)
                                        (fun () ->
                                            addTimestamp
                                                (fun () ->
                                                    "[ ||==> setAdapterValue ](j4) invoking debouncedSetAtom. gun inside setAtom passed to debouncedBatchPutFromUi  ")
                                                getDebugInfo

                                            debouncedAdapterSetAtom (false, (lastTicks, lastValue)))


                            Some setAdapterValue
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
                        | Some gunAtomNode -> gunAtomNode.off () |> ignore
                        | _ -> ()

                        let getDebugInfo () =
                            $"adapterOptions={adapterOptions} {getDebugInfo ()}"

                        addTimestamp (fun () -> "[ <==== unmountFn ](j5) gun unmount ") getDebugInfo

                    | _ -> ())
            | Atom.AdapterType.Hub ->
                (fun _storeAtomPath _getter _setter adapterOptions _setValue ->
                    match adapterOptions with
                    | Atom.AdapterOptions.Hub (_alias, _hubUrl) ->
                        //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                        Profiling.addTimestamp (fun () -> $"+09B ====> getAtomAdapter hub mount  {getDebugInfo ()}  ")

                        Some
                            (fun (_lastTicks, _lastValue) ->
                                Profiling.addTimestamp
                                    (fun () -> $"+09-1B ====> getAtomAdapter hub setAdapterValue  {getDebugInfo ()}  "))
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

                        Some
                            (fun (_lastTicks, _lastValue) ->
                                let getDebugInfo () =
                                    $"_lastTicks={_lastTicks} _lastValue={_lastValue} {getDebugInfo ()}"

                                addTimestamp
                                    (fun () -> "[ ¨¨ setAdapterValue ](j8) memory inside debouncedPutFromUi setAtom ")
                                    getDebugInfo)
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


    let inline createAtomWithAdapter adapterType mount unmount : AtomConfig<(bool * (TicksGuid * 'A3)) option> =
        let atom = Atom.create (AtomType.Atom None)

        let mutable setAdapterValue = None

        let getDebugInfo () =
            $" adapterType={adapterType} atom={atom} setAdapterValue.IsSome={setAdapterValue.IsSome}  {getDebugInfo ()}"

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
                | Some (newFromUi, (newTicks, newValue: 'A3)) ->
                    let getDebugInfo () =
                        $" newTicks={newTicks} newValue={newValue} newFromUi={newFromUi} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ (^^^^1) wrapper.set() ](f3-2) ") getDebugInfo

                    Atom.change
                        setter
                        atom
                        (fun oldValue ->
                            let getDebugInfo () =
                                $"oldValue={oldValue} {getDebugInfo ()}"

                            match setAdapterValue with
                            | Some setAdapterValue when
                                (match oldValue with
                                 | None -> true
                                 | Some (_, (_, oldValue)) when oldValue |> Object.compare newValue |> not -> true
                                 | _ -> false)
                                ->
                                addTimestamp
                                    (fun () -> "[ (^^^^2) wrapper.set() ](f3) triggering new adapter value")
                                    getDebugInfo

                                // gunPut
                                setAdapterValue (newFromUi, (newTicks, newValue))
                            | _ ->
                                addTimestamp
                                    (fun () -> "[ (^^^^2) wrapper.set() ](f3-1) skipping new adapter assign")
                                    getDebugInfo

                            Some (newFromUi, (newTicks, newValue)))
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
                        $"setAdapterValue.IsNone={setAdapterValue.IsNone} adapterOptions={Json.encodeWithNull adapterOptions} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ @@> mount ](f6)") getDebugInfo

                    if setAdapterValue.IsNone then
                        setAdapterValue <- mount getter setter adapterOptions (fun x -> setAtom (Some x))
                })
            (fun getter setter adapterOptions ->
                let getDebugInfo () =
                    $"setAdapterValue.IsNone={setAdapterValue.IsNone} adapterOptions={Json.encodeWithNull adapterOptions} {getDebugInfo ()}"

                addTimestamp (fun () -> "[ <@@ unmount ](f7)") getDebugInfo

                if setAdapterValue.IsSome then
                    unmount getter setter adapterOptions
                    setAdapterValue <- None)

    let inline subscribeFamilyKey<'TKey, 'A4 when 'TKey: equality and 'A4: equality>
        (_atomFamily: 'TKey -> AtomConfig<'A4>)
        =
        let result: AtomConfig<AtomConfig<'TKey> []> = Atom.create (AtomType.Atom [||])
        result

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

    let inline createRegisteredAtomWithGroup<'TGroup, 'A6>
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
                    let userAtom = userAtomFamily (GroupRef (string defaultGroup), alias, storeAtomPath)
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

                    let getDebugInfo () =
                        $"alias={alias} userAtom={userAtom} filteredResult={Json.encodeWithNull filteredResult} {getDebugInfo ()} "

                    addTimestamp (fun () -> "[ wrapper.get() ](d2)") getDebugInfo

                    filteredResult
                    |> List.map
                        (fun (groupRef, (ticks, atomValueRef: AtomValueRef)) ->
                            groupRef |> unbox<'TGroup>, (ticks, atomValueRef |> unbox<'A6>))
                    |> function
                        | [] ->
                            [
                                defaultGroup, (Guid.Empty, defaultValue)
                            ]
                        | result -> result)
                (fun getter setter (newValueFn: ('TGroup * (TicksGuid * 'A6)) list) -> // TODO:log fn, hook
                    let alias = Atom.get getter Selectors.Gun.alias
                    let userAtom = groupMapFamily (GroupRef (string defaultGroup), alias, storeAtomPath)

                    let getDebugInfo () =
                        $"alias={alias} userAtom={userAtom} {getDebugInfo ()} "

                    addTimestamp (fun () -> "[ wrapper.set() ](d3)") getDebugInfo

                    Atom.change
                        setter
                        userAtom
                        (fun oldValue ->
                            (newValueFn
                             |> unbox<('TGroup * (TicksGuid * 'A6)) list -> ('TGroup * (TicksGuid * 'A6)) list>)
                                (
                                    oldValue
                                    |> List.map
                                        (fun (group, (ticks, value)) ->
                                            group |> unbox<'TGroup>, (ticks, value |> unbox<'A6>))
                                )
                            |> List.map
                                (fun (group, (ticks, value)) ->
                                    GroupRef (string group), (ticks, AtomValueRef (box value)))))
            |> Atom.register storeAtomPath

        wrapper?init <- defaultValue

        wrapper

    let adapterAtomMap =
        Dictionary<Atom.AdapterType * Gun.Alias * StoreAtomPath, AtomConfig<(bool * (TicksGuid * AtomValueRef)) option>>
            ()

    let inline getAdapterValues atomType getter storeAtomPath =
        let alias = Atom.get getter Selectors.Gun.alias

        let getDebugInfo () =
            $"atomType={atomType} alias={alias} storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath} {getDebugInfo ()}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.getAdapterValues {fn ()} | {getDebugInfo ()}")

        Reflection.unionCases<Atom.AdapterType>
        |> List.choose
            (fun adapterType ->
                match alias, getAdapterOptions getter adapterType with
                | Some alias, Some adapterOptions ->

                    let adapterAtom: AtomConfig<(bool * (TicksGuid * 'A7)) option> =
                        match adapterAtomMap.TryGetValue ((adapterType, alias, storeAtomPath)) with
                        | true, (value: AtomConfig<(bool * (TicksGuid * AtomValueRef)) option>) ->
                            value
                            |> unbox<AtomConfig<(bool * (TicksGuid * 'A7)) option>>
                        | _ ->
                            let mount, unmount = getAdapterSubscription atomType adapterType
                            let mount, unmount = mount storeAtomPath, unmount storeAtomPath

                            let newAtom: AtomConfig<(bool * (TicksGuid * 'A7)) option> =
                                createAtomWithAdapter adapterType mount unmount

                            adapterAtomMap.Add (
                                (adapterType, alias, storeAtomPath),
                                newAtom
                                |> unbox<AtomConfig<(bool * (TicksGuid * AtomValueRef)) option>>
                            )

                            newAtom

                    let adapterValue = Atom.get getter adapterAtom

                    let getDebugInfo () =
                        $"adapterType={adapterType} adapterAtom={adapterAtom} adapterOptions={adapterOptions} adapterValue={adapterValue} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ getAdapterValues ](a4) returning valid adapter") getDebugInfo

                    Some (adapterType, adapterOptions, adapterAtom, adapterValue)
                | _ -> None)


    let inline sync<'A when 'A: equality>
        (adapterValues: (Atom.AdapterType * Atom.AdapterOptions * AtomConfig<(bool * (TicksGuid * 'A)) option> * (bool * (TicksGuid * 'A)) option) list)
        (atom: AtomConfig<((Atom.AdapterType * bool) * (TicksGuid * 'A)) list>)
        : unit =
        match lastStore with
        | Some (getter, setter) ->
            let getDebugInfo () =
                $"atom={atom} adapterValues={Json.encodeWithNull adapterValues} {getDebugInfo ()}"

            let addTimestamp fn getDebugInfo =
                Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.sync {fn ()} | {getDebugInfo ()}")

            let localAdapters = Atom.get getter atom

            let (lastAdapterType: Atom.AdapterType, lastFromUi: bool), (lastTicks: TicksGuid, lastValue) =
                localAdapters |> List.head

            let getDebugInfo () =
                $" lastAdapterType={lastAdapterType} lastFromUi={lastFromUi} lastTicks={lastTicks} lastValue={lastValue} == [ localAdapters={Json.encodeWithNull localAdapters} ] {getDebugInfo ()} "

            addTimestamp (fun () -> "[ sync ](c1-1) init. from debounce") getDebugInfo

            let values =
                adapterValues
                |> List.choose
                    (function
                    | adapterType, _adapterOptions, adapterAtom, Some (fromUi, (adapterTicks, adapterValue)) ->
                        let result =
                            (fun (fromUi, (ticks, newValue)) ->
                                let getDebugInfo () =
                                    $"adapterType={adapterType} adapterTicks={adapterTicks} adapterValue={adapterValue} ticks={ticks} newValue={newValue} {getDebugInfo ()} "

                                addTimestamp
                                    (fun () -> "[ (:::::) adapter.write() ](c1) invoking Atom.set")
                                    getDebugInfo

                                Atom.set setter adapterAtom (Some (fromUi, (ticks, newValue)))),
                            (adapterType, fromUi),
                            (adapterTicks, adapterValue)

                        Some result
                    | _ -> None)

            let values =
                values
                |> List.append [
                    (fun (fromUi, (ticks, newValue)) ->
                        addTimestamp
                            (fun () -> "[ (:::::) lastAdapter.write() ](c2) invoking Atom.change ")
                            getDebugInfo

                        Atom.change
                            setter
                            atom
                            (fun oldValue ->
                                let getDebugInfo () =
                                    $"lastAdapterType={lastAdapterType} oldValue={oldValue} ticks={ticks} newValue={newValue} {getDebugInfo ()} "

                                addTimestamp
                                    (fun () -> "[ (:::::) lastAdapter.write() ](c2) inside Atom.change ")
                                    getDebugInfo

                                ((lastAdapterType, fromUi), (ticks, newValue))
                                :: (oldValue
                                    |> List.filter (fun ((adapterType, _), _) -> adapterType <> lastAdapterType)))),
                    (lastAdapterType, true),
                    (lastTicks, lastValue)
                   ]
                |> List.sortByDescending (fun (_, _, (ticks, _)) -> ticks)

            let valuesfmt =
                Json.encodeWithNull (
                    values
                    |> List.map
                        (fun (_setAdapterAtom, (adapterType, fromUi), (ticks, value)) ->
                            "adapterType", adapterType, "fromUi", fromUi, "ticks", ticks, "value", value)
                )

            let getDebugInfo () =
                $" {getDebugInfo ()} values={valuesfmt} {getDebugInfo ()}"

            let lastAdapterSetAtom, (lastAdapterType, lastFromUi), (lastTicks, lastValue) = values.Head


            values
            |> List.skip 1
            |> List.filter (fun (_, _, (_, value)) -> value |> Object.compare lastValue |> not)
            |> List.map
                (fun (setAdapterAtom, (adapterType, fromUi), (ticks, value)) ->
                    promise {
                        let getDebugInfo () =
                            $" adapterFromUi={fromUi} lastFromUi={lastFromUi} adapterType={adapterType} lastAdapterType={lastAdapterType} lastTicks={lastTicks} ticks={ticks} lastValue={lastValue} value={value} {getDebugInfo ()} "

                        if lastTicks = ticks then
                            addTimestamp
                                (fun () -> "[ (%%%%) invalidAdapter.write() ](c3) same ticks. skipping")
                                getDebugInfo
                        elif lastTicks > ticks then
                            // set adapter value from local atom

                            addTimestamp (fun () -> "[ (%%%%) invalidAdapter.write() ](c4)") getDebugInfo
                            setAdapterAtom (lastFromUi, (lastTicks, lastValue))
                        else
                            addTimestamp
                                (fun () ->
                                    "[ invalidAdapter.write() ](c5) (%%%%) assigning current atom. adapter is newer (probably wont invoke)")
                                getDebugInfo

                            // set local atom from adapter value
                            lastAdapterSetAtom (fromUi, (ticks, value))

                        ()
                    })
            |> List.toArray
            |> Promise.all
            |> Promise.ignore
            |> Promise.start
        | _ -> ()


    let inline createRegisteredAtomWithSubscription<'A8 when 'A8: equality>
        storeAtomPath
        (defaultValue: 'A8)
        : AtomConfig<'A8> =
        typeMetadataMap.[typeof<'A8>] <-
            {|
                DefaultValue = AtomValueRef (box defaultValue)
                Decode = unbox Gun.userDecode<TicksGuid * 'A8>
                Encode = unbox Gun.userEncode<TicksGuid * 'A8>
            |}

        let atomType = typeof<'A8>

        let localAdaptersAtom
        //            : AtomConfig<((Atom.AdapterType * bool) * (TicksGuid * 'A8)) list>
         = createRegisteredAtomWithGroup storeAtomPath ((Atom.AdapterType.Memory, false), defaultValue)

        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

        let getDebugInfo () =
            $"localAdaptersAtom={localAdaptersAtom} atomPath={atomPath} defaultValue={defaultValue} {getDebugInfo ()}"

        let addTimestamp fn getDebugInfo =
            Profiling.addTimestamp
                (fun () ->
                    $"{nameof FsStore} | Engine.createRegisteredAtomWithSubscription {fn ()} | {getDebugInfo ()}")

        addTimestamp (fun () -> "[ constructor ](a1)") getDebugInfo

        let debouncedSync =
            Js.debounce
                (fun adapterValues ->
                    addTimestamp (fun () -> "[ debouncedSync ](a2)") getDebugInfo
                    sync adapterValues localAdaptersAtom)
                0

        let inline refreshAdapterValues getter =
            addTimestamp (fun () -> "[ refreshAdapterValues ](a3)") getDebugInfo
            let adapterValues = getAdapterValues atomType getter storeAtomPath
            debouncedSync adapterValues

        let rec wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    refreshAdapterValues getter
                    let localAdapters = Atom.get getter localAdaptersAtom
                    let _ticks, (_adapterType, value) = localAdapters |> List.head


                    let getDebugInfo () =
                        $"_ticks={_ticks} _adapterType={_adapterType} value={value} {getDebugInfo ()}"

                    addTimestamp (fun () -> "[ wrapper.get() ](a5)") getDebugInfo

                    value)
                (fun _getter setter (newValue: 'A8) ->
//                    refreshAdapterValues getter

                    Atom.change
                        setter
                        localAdaptersAtom
                        (fun localAdapters ->
                            let newItem = (Atom.AdapterType.Memory, true), (Guid.newTicksGuid (), newValue)

                            let getDebugInfo () = $"newItem={newItem} {getDebugInfo ()}"

                            addTimestamp (fun () -> "[ <&¨<->¨&> wrapper.set() ](a6)") getDebugInfo

                            localAdapters
                            |> List.filter (fun ((adapterType, _), _) -> adapterType <> Atom.AdapterType.Memory)
                            |> List.append (newItem |> List.singleton)))

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



    let inline createRegisteredAtomWithSubscriptionStorage<'A10 when 'A10: equality>
        storeAtomPath
        (defaultValue: 'A10)
        =
        //        let storageAtom = Atom.createRegisteredWithStorage storeAtomPath (Guid.Empty, defaultValue)
        let storageAtom = Atom.createRegisteredWithStorage<'A10> storeAtomPath defaultValue
        let syncAtom = createRegisteredAtomWithSubscription<'A10> storeAtomPath defaultValue
        bindAtom<'A10> syncAtom storageAtom

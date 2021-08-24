namespace FsStore

open Fable.Core
open FsJs
open FsStore.Bindings.Jotai
open FsStore.Model
open FsStore
open FsStore.State
open FsCore

#nowarn "40"


module Engine =
    type UpdateFn<'State, 'Command, 'Event> =
        Getter<obj> -> Setter<obj> -> 'State -> 'Command -> JS.Promise<'State * Message<'Command, 'Event> list>

    let inline consumeCommands (updateFn: UpdateFn<_, _, _>) state getter setter commands =
        promise {
            let logger = Atom.get getter Selectors.logger

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

            logger.Trace
                (fun () ->
                    $"Messaging.consumeMessages commands={commands} newState={newState} processedMessages={processedMessages}")

            return
                newState,
                processedMessages
                |> List.choose
                    (function
                    | Message.Event event -> Some event
                    | _ -> None)
        }


    let mutable lastStore: (Getter<obj> * Setter<obj>) option = None


    let inline wrapAtomWithState<'A, 'S>
        (stateFn: Getter<obj> -> 'S)
        (mount: Getter<obj> -> Setter<obj> -> 'S -> ('A -> unit) -> JS.Promise<unit>)
        (unmount: Getter<obj> -> Setter<obj> -> 'S -> unit)
        (atom: AtomConfig<'A>)
        =
        let storeAtomPath =
            if Atom.isRegistered (AtomReference.Atom atom) then
                Some (Atom.query (AtomReference.Atom atom))
            else
                None

        let mutable lastState = None

        let getDebugInfo () =
            $" | atom={atom} storeAtomPath={storeAtomPath |> Option.map StoreAtomPath.AtomPath} lastState={Json.encodeWithNull lastState} "

        Logger.logTrace (fun () -> $"Engine.wrapAtom [ constructor ] {getDebugInfo ()}")

        let refreshInternalState getter =
            if lastStore.IsNone then lastStore <- Atom.get getter Selectors.store

            let logger = Atom.get getter Selectors.logger
            Logger.State.lastLogger <- logger

            lastState <- Some (stateFn getter)

        let wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    refreshInternalState getter

                    let result = Atom.get getter atom

                    Profiling.addTimestamp $"Engine.wrapAtomWithState wrapper.get() {getDebugInfo ()}"

                    result)
                (fun getter setter newValue ->
                    refreshInternalState getter

                    Profiling.addTimestamp
                        $"Engine.wrapAtomWithState wrapper.set() newValue={newValue} {getDebugInfo ()}"

                    Atom.set setter atom newValue)
            |> Atom.addSubscription
                true
                (fun setAtom ->
                    promise {
                        Profiling.addTimestamp $"Engine.wrapAtomWithState onMount() {getDebugInfo ()}"

                        match lastState, lastStore with
                        | Some state, Some (getter, setter) -> do! mount getter setter state setAtom
                        | _ -> failwith "wrap atom: invalid state"
                    })
                (fun () ->
                    Profiling.addTimestamp $"Engine.wrapAtom onUnmount() {getDebugInfo ()}"

                    match lastState, lastStore with
                    | Some state, Some (getter, setter) -> unmount getter setter state
                    | _ -> failwith "wrap atom: invalid state unsub")

        match storeAtomPath with
        | Some storeAtomPath -> wrapper |> Atom.register storeAtomPath
        | None -> wrapper


    let inline wrapAtom<'A>
        (mount: Getter<obj> -> Setter<obj> -> ('A -> unit) -> JS.Promise<unit>)
        (unmount: Getter<obj> -> Setter<obj> -> unit)
        (atom: AtomConfig<'A>)
        =
        wrapAtomWithState
            (fun _ -> ())
            (fun getter setter _ -> mount getter setter)
            (fun getter setter _ -> unmount getter setter)
            atom


    let inline wrapAtomWithInterval defaultValue interval atom =
        let mutable intervalHandle = -1
        let mutable lastValue = None

        let getDebugInfo () =
            $" | readSelectorInterval baseInfo: interval={interval} defaultValue={defaultValue} lastValue={lastValue} timeout={intervalHandle} "

        let cache = Atom.Primitives.atom defaultValue

        atom
        |> wrapAtom
            (fun getter setter _setAtom ->
                promise {
                    let logger = Logger.State.lastLogger
                    logger.Trace (fun () -> $"Store.readSelectorInterval. onMount() {getDebugInfo ()}")

                    let fn () =
                        logger.Trace (fun () -> $"Store.readSelectorInterval. #1 timeout {getDebugInfo ()}")

                        if intervalHandle >= 0 then
                            let atomValue = Atom.get getter atom

                            if Some atomValue |> Object.compare lastValue |> not then
                                logger.Trace
                                    (fun () ->
                                        $"Store.readSelectorInterval. #2 timeout atomValue={atomValue |> Option.isSome} {getDebugInfo ()}")

                                Atom.set setter cache atomValue
                                lastValue <- Some atomValue

                    if intervalHandle = -1 then fn ()
                    intervalHandle <- JS.setInterval fn interval
                })
            (fun _getter _setter ->
                let logger = Logger.State.lastLogger
                logger.Trace (fun () -> $"atomWithDelay onUnmount() {getDebugInfo ()}")

                if intervalHandle >= 0 then JS.clearTimeout intervalHandle
                intervalHandle <- -1)

    let inline wrapAtomWithSubscription mount unmount atom =
        let getDebugInfo state =
            $"""
        | atom={atom} state={Json.encodeWithNull state} """

        let storeAtomPath = Atom.query (AtomReference.Atom atom)
        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

        atom
        |> wrapAtomWithState
            (fun getter ->
                let alias = Atom.get getter Selectors.Gun.alias
                let gunOptions = Atom.get getter Atoms.gunOptions
                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))

                {|
                    Alias = alias
                    GunOptions = gunOptions
                    GunAtomNode = gunAtomNode
                |})
            (fun _getter _setter state _setAtom ->
                promise {
                    Profiling.addTimestamp $"Engine.wrapAtomWithSubscription. onMount() {getDebugInfo state}"

                    do! mount ()
                })
            (fun _getter _setter state ->
                Profiling.addTimestamp $"Engine.wrapAtomWithSubscription onUnmount() {getDebugInfo state}"
                unmount ()
                ())

    let inline subscribeCollection<'T> storeRoot collection (_onFormat: TicksGuid -> 'T) =
        let storeAtomPath = CollectionAtomPath (storeRoot, collection)
        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath
        let getDebugInfo () = $"atomPath={atomPath}"

        Atom.createRegistered storeAtomPath (AtomType.Atom ([||]: 'T []))
        |> wrapAtomWithSubscription
            (fun () ->
                promise {
                    Profiling.addTimestamp $"@@> subscribeCollection mount {getDebugInfo ()}"
                })
            (fun () ->
                Profiling.addTimestamp $"<@@ subscribeCollection unmount {getDebugInfo ()} "
                )
        |> Atom.split

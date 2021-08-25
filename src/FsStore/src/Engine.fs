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
        (stateFn: Getter<obj> -> 'S option)
        (mount: Getter<obj> -> Setter<obj> -> 'S -> ('A -> unit) -> JS.Promise<unit>)
        (unmount: Getter<obj> -> Setter<obj> -> 'S -> unit)
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
            $" | atom={atom} mounted={mounted} storeAtomPath={storeAtomPath |> Option.map StoreAtomPath.AtomPath} lastState={Json.encodeWithNull lastState} "

        Logger.logTrace (fun () -> $"Engine.wrapAtom [ constructor ] {getDebugInfo ()}")

        let mutable lastSetAtom = fun _ -> failwith "invalid lastSetAtom"

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
                Profiling.addTimestamp $"Engine.wrapAtomWithState newMount() {getDebugInfo ()}"

                match getState () with
                | Some (getter, setter, state) ->
                    mounted <- true
                    do! mount getter setter state lastSetAtom
                | None -> ()
            }

        let newUnmount () =
            if mounted then
                Profiling.addTimestamp $"Engine.wrapAtom onUnmount() {getDebugInfo ()}"

                match getState () with
                | Some (getter, setter, state) ->
                    mounted <- false
                    unmount getter setter state

                    JS.setTimeout (fun () ->
                        Profiling.addTimestamp $"Engine.wrapAtom onUnmount() clearing lastState {getDebugInfo ()}"
                        lastState <- None) 0
                    |> ignore
                | None -> ()

        let refreshInternalState getter =
            if lastStore.IsNone then lastStore <- Atom.get getter Selectors.store

            let logger = Atom.get getter Selectors.logger
            Logger.State.lastLogger <- logger

            let newState = stateFn getter

            match newState with
            | Some _ ->
                Profiling.addTimestamp $"Engine.wrapAtomWithState refreshInternalState. mount {getDebugInfo ()}"
                lastState <- newState
                newMount () |> Promise.start
            | None -> newUnmount ()


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
                    lastSetAtom <- setAtom
                    newMount ())
                (fun () -> newUnmount ())

        match storeAtomPath with
        | Some storeAtomPath -> wrapper |> Atom.register storeAtomPath
        | None -> wrapper


    let inline wrapAtom<'A>
        (mount: Getter<obj> -> Setter<obj> -> ('A -> unit) -> JS.Promise<unit>)
        (unmount: Getter<obj> -> Setter<obj> -> unit)
        (atom: AtomConfig<'A>)
        =
        wrapAtomWithState
            (fun _ -> Some ())
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

    let inline wrapAtomWithSubscription stateFn mount unmount atom =
        let storeAtomPath = Atom.query (AtomReference.Atom atom)
        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

        let getDebugInfo () =
            $"""
        | atom={atom} atomPath={atomPath} """


        atom
        |> wrapAtomWithState
            (fun getter ->
                let newState = stateFn getter
                newState)
            (fun _getter _setter state _setAtom ->
                promise {
                    Profiling.addTimestamp $"Engine.wrapAtomWithSubscription. onMount(). {getDebugInfo ()}"
                    do! mount state
                })
            (fun _getter _setter _state ->
                Profiling.addTimestamp $"Engine.wrapAtomWithSubscription onUnmount() {getDebugInfo ()}"
                unmount ()
                ())

    let inline subscribeCollection<'T> storeRoot collection (_onFormat: TicksGuid -> 'T) =
        let storeAtomPath = CollectionAtomPath (storeRoot, collection)
        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath
        let getDebugInfo () = $"atomPath={atomPath}"

        Atom.createRegistered storeAtomPath (AtomType.Atom ([||]: 'T []))
        |> wrapAtomWithSubscription
            (fun getter ->
                let alias = Atom.get getter Selectors.Gun.alias
                let gunOptions = Atom.get getter Atoms.gunOptions
                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))

                Profiling.addTimestamp $"* subscribeCollection stateFn {getDebugInfo ()}"

                match gunOptions, alias, gunAtomNode with
                | GunOptions.Sync peers, Some alias, Some gunAtomNode -> Some (peers, alias, gunAtomNode)
                | _ -> None)
            (fun state ->
                promise { Profiling.addTimestamp $"@@> subscribeCollection mount state={state} {getDebugInfo ()}" })
            (fun () -> Profiling.addTimestamp $"<@@ subscribeCollection unmount {getDebugInfo ()} ")
        |> Atom.split

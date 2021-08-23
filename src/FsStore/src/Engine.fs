namespace FsStore

open FsCore.BaseModel
open Fable.Core
open FsStore.Bindings.Jotai
open FsStore.Model
open FsStore
open FsStore.State
open FsCore

#nowarn "40"


module rec Engine =
    let collection = Collection (nameof Engine)


    let rec appState =
        Atom.Primitives.atomFamily
            (fun (deviceId: DeviceId) ->
                Atom.createRegistered
                    (IndexedAtomPath (
                        storeRoot,
                        collection,
                        Atoms.Device.deviceIdIdentifier deviceId,
                        AtomName (nameof appState)
                    ))
                    (AtomType.Atom AppEngineState.Default))


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

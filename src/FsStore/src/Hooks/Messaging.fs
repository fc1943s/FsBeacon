namespace FsStore.Hooks

open FsJs
open FsStore
open FsStore.Model
open FsCore
open FsStore.Bindings


module Messaging =
    let inline appUpdate getter setter state command =
        promise {
            let logger = Store.value getter Selectors.logger

            let! result =
                promise {
                    match command with
                    | AppCommand.Init state -> return state, []
                    | AppCommand.SignInPair keys ->
                        match! Auth.signIn getter setter ("", keys |> Json.encode<Gun.GunKeys>) with
                        | Ok _ ->
                            return
                                state,
                                Message.Event AppEvent.UserSignedIn
                                |> List.singleton
                        | Error error ->
                            return
                                state,
                                Message.Event (AppEvent.Error error)
                                |> List.singleton
                    | AppCommand.RegisterAdapter adapter ->
                        return
                            { state with
                                Adapters = adapter :: state.Adapters
                            },
                            []
                //                    | _ -> return failwith "invalid message"
                }

            Profiling.addCount $"Messaging.appUpdate. command={command} result={result}"
            logger.Trace (fun () -> $"Messaging.appUpdate. command={command} result={result}")
            return result
        //                    | _ -> return failwith "invalid message"
        }

    let inline atomUpdate getter setter state command =
        promise {
            let logger = Store.value getter Selectors.logger

            let! result =
                promise {
                    match command with
                    | AtomCommand.Init state -> return state, []
                    | AtomCommand.Subscribe -> return state, []
                    | AtomCommand.Unsubscribe -> return state, []
                //                    | _ -> return failwith "invalid message"
                }

            Profiling.addCount $"Messaging.atomUpdate. command={command} result={result}"
            logger.Trace (fun () -> $"Messaging.atomUpdate. command={command} result={result}")
            return result
        //                    | _ -> return failwith "invalid message"
        }

namespace FsStore.Hooks

open FsJs
open FsStore
open FsStore.Hooks
open FsStore.Model
open FsCore
open FsStore.Bindings


module Messaging =
    let useMessageProcessor () =
        let logger = Store.useValue Selectors.logger
        let signIn = Auth.useSignIn ()

        Store.useCallbackRef
            (fun _ _ (ack, onAck, message) ->
                promise {
                    match ack, message with
                    | Some false,
                      Message.Command (Command.KeySignIn ({
                                                              priv = Some (Gun.Priv (String.ValidString _))
                                                          } as keys)) ->
                        match! signIn ("", keys |> Json.encode<Gun.GunKeys>) with
                        | Ok _ ->
                            logger.Debug (fun () -> "MessageProcessor keySignIn ack")
                            onAck ()
                        | Error error -> logger.Error (fun () -> $"MessageProcessor keySignIn error={error}")
                    | _ -> ()
                })

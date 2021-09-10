namespace FsBeacon.Template.Components

open FsStore
open FsCore
open Fable.React
open Feliz
open FsJs
open FsStore.Hooks
open FsStore.Model
open FsStore.State
open FsBeacon.Template.State
open FsUi.Bindings
open FsUi.Hooks
open FsUi.State


module MessageConsumer =
    [<ReactComponent>]
    let MessageConsumer messageIdAtom =
        let logger = Store.useValue Selectors.logger
        let deviceInfo = Store.useValue Selectors.deviceInfo
        let appState = Store.useValue (Atoms.Device.appState deviceInfo.DeviceId)
        let consumeCommands = Store.useCallbackRef (Engine.consumeCommands Messaging.appUpdate appState)
        let messageId = Store.useValue messageIdAtom
        let appMessage = Store.useValue (Atoms.Message.appMessage messageId)
        let ack, setAck = Store.useState (Atoms.Message.ack messageId)


        let getLocals () =
            $"messageId={messageId} ack={ack} appMessage={appMessage} {getLocals ()}"

        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | MessageConsumer [ render ]") getLocals

        React.useEffect (
            (fun () ->
                promise {
                    match ack with
                    | Some false ->
                        match appMessage with
                        | Message.Command command ->
                            Profiling.addTimestamp
                                (fun () ->
                                    $"{nameof FsBeacon} | MessageConsumer [ render ] starting consumeCommands...")
                                getLocals

                            let! events = consumeCommands (command |> List.singleton)

                            let getLocals () =
                                $"command={command} events={events} {getLocals ()}"

                            logger.Info
                                (fun () -> $"{nameof FsBeacon} | MessageConsumer. command processed. acked")
                                getLocals

                            setAck (Some true)
                        | _ -> failwith $"{nameof FsBeacon} | MessageConsumer. invalid command"

                    | _ -> ()
                }
                |> Promise.start),
            [|
                box logger
                box consumeCommands
                box appMessage
                box ack
                box setAck
            |]
        )

        nothing

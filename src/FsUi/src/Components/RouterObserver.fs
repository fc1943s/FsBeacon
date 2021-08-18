namespace FsUi.Components

open Fable.Core
open FsCore
open FsJs
open Feliz.Router
open Feliz
open FsStore
open FsStore.Model
open FsStore.Hooks
open FsUi.Bindings
open Fable.Core.JsInterop
open Microsoft.FSharp.Core.Operators


module RouterObserver =
    [<ReactComponent>]
    let rec RouterWrapper children =
        let logger = Store.useValue Selectors.logger
        let alias = Store.useValue Selectors.Gun.alias
        logger.Debug (fun () -> "RouterObserver.render: Constructor")

        let deviceInfo = Store.useValue Selectors.deviceInfo

        let lastSegments = React.useRef []
        let messageProcessor = Messaging.useMessageProcessor ()

        React.useEffect (
            (fun () ->
                match Dom.window () with
                | Some window ->
                    let redirect = window.sessionStorage?redirect
                    emitJsExpr () "delete sessionStorage.redirect"

                    match redirect with
                    | String.Valid _ when redirect <> window.location.href ->
                        Router.navigatePath (redirect |> String.split "/" |> Array.skip 3)
                    | _ -> ()
                | None -> ()),
            [||]
        )

        let onChange =
            Store.useCallbackRef
                (fun _ setter (newSegments: string list) ->
                    promise {
                        if newSegments <> lastSegments.current then
                            logger.Debug
                                (fun () ->
                                    $"RouterObserver. onChange 1.
newSegments={JS.JSON.stringify newSegments}
lastSegments.current={JS.JSON.stringify lastSegments.current} ")

                            lastSegments.current <- newSegments
                            Store.change setter Atoms.routeTrigger ((+) 1)

                            let messages =
                                match newSegments with
                                | [ base64 ] ->
                                    try

                                        let json =
                                            match Dom.window () with
                                            | Some window -> window?atob base64
                                            | None -> ""

                                        json |> Json.decode<Message []> |> Some
                                    with
                                    | ex ->
                                        logger.Error
                                            (fun () ->
                                                $"RouterObserver. onChange 3.
error deserializing. ex={ex}
newSegments={JS.JSON.stringify newSegments} ")

                                        Some [||]
                                | _ -> None

                            match messages with
                            | Some [||] -> logger.Error (fun () -> $"Invalid messages. newSegments={newSegments}")
                            | Some messages ->
                                logger.Debug
                                    (fun () ->
                                        $"RouterObserver. onChange 2. saving messages.
                                                  messages={messages}
                                                  newSegments={JS.JSON.stringify newSegments} ")

                                do!
                                    messages
                                    |> Array.map
                                        (fun message ->
                                            promise {
                                                match alias with
                                                | Some _ ->
                                                    let messageId = MessageId.NewId ()
                                                    Store.set setter (State.Atoms.Message.message messageId) message
                                                    Store.set setter (State.Atoms.Message.ack messageId) (Some false)
                                                | None ->
                                                    let ack = Some false

                                                    let onAck () =
                                                        logger.Debug
                                                            (fun () ->
                                                                $"RouterObserver. acked.
                                                                                          essage={message}
                                                                                          newSegments={JS.JSON.stringify newSegments} ")

                                                    do! messageProcessor (ack, onAck, message)
                                            })
                                    |> Promise.all
                                    |> Promise.ignore

                                Router.navigatePath [||]
                            | None -> Router.navigatePath [||]
                    })

        Store.useHashedEffectOnce
            (nameof RouterWrapper, deviceInfo.DeviceId)
            (fun _getter _setter ->
                promise {
                    logger.Info (fun () -> $"RouterObserver useHashedEffectOnce. {Browser.Dom.window.location.href}")

                    match Browser.Dom.window.location.hash with
                    | null
                    | "" -> ()
                    | hash when hash.StartsWith "#" ->
                        let newSegment = hash |> String.substringFrom 1
                        do! onChange [ newSegment ]
                    | _ -> ()
                })


        //        let setSessionRestored = Store.useSetState Atoms.Session.sessionRestored
//
//        React.useEffect (
//            (fun () -> setSessionRestored true),
//            [|
//                box setSessionRestored
//            |]
//        )

        React.router [
            router.hashMode
            router.onUrlChanged (onChange >> Promise.start)
            router.children [ yield! children ]
        ]

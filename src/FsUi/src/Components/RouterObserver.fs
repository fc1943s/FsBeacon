namespace FsUi.Components

open Fable.Core
open FsCore
open FsJs
open Feliz.Router
open Feliz
open FsStore
open FsStore.Model
open FsStore.Hooks
open FsStore.State
open FsStore.Store.SyncEngine
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
        let appState = Store.useValue (Engine.appState deviceInfo.DeviceId)
        let consumeCommands = Store.useCallbackRef (Engine.consumeCommands Messaging.appUpdate appState)

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
                            Atom.change setter Atoms.routeTrigger ((+) 1)

                            let messages =
                                match newSegments with
                                | [ base64 ] ->
                                    try

                                        let json =
                                            match Dom.window () with
                                            | Some window -> window?atob base64
                                            | None -> ""

                                        json
                                        |> Json.decode<Message<AppCommand, AppEvent> list>
                                        |> Some
                                    with
                                    | ex ->
                                        logger.Error
                                            (fun () ->
                                                $"RouterObserver. onChange 3.
error deserializing. ex={ex}
newSegments={JS.JSON.stringify newSegments} ")

                                        Some []
                                | _ -> None

                            match messages with
                            | Some [] -> logger.Error (fun () -> $"Invalid messages. newSegments={newSegments}")
                            | Some messages ->
                                logger.Debug
                                    (fun () ->
                                        $"RouterObserver. onChange 2. saving messages.
                                                  messages={messages}
                                                  newSegments={JS.JSON.stringify newSegments} ")

                                match alias with
                                | Some _ ->
                                    messages
                                    |> List.iter
                                        (fun message ->
                                            let messageId = Hydrate.hydrateAppMessage setter message

                                            logger.Debug
                                                (fun () -> $"RouterObserver. message hydrated. messageId={messageId} "))
                                | None ->
                                    let commands =
                                        messages
                                        |> List.choose
                                            (function
                                            | Message.Command command -> Some command
                                            | _ -> None)


                                    let! events = consumeCommands commands

                                    logger.Debug
                                        (fun () -> $"RouterObserver. no alias. consumed inline. events={events}  ")

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

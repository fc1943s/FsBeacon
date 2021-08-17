namespace FsUi.Components

open Fable.Core
open FsCore
open FsJs
open Fable.Core.JsInterop
open Feliz.Router
open Feliz
open FsStore
open FsUi.Bindings


module RouterObserver =

    type RouteOperation = | RawDataEntry

    type W = RouteOperation list

    [<ReactComponent>]
    let RouterWrapper children =
        let logger = Store.useValue Selectors.logger
        logger.Debug (fun () -> "RouterObserver.render: Constructor")

        React.useEffect (
            (fun () ->
                match Dom.window () with
                | Some window ->
                    let redirect = window.sessionStorage?redirect
                    emitJsExpr () "delete sessionStorage.redirect"

                    match redirect with
                    | String.ValidString _ when redirect <> window.location.href ->
                        Router.navigatePath (redirect |> String.split "/" |> Array.skip 3)
                    | _ -> ()
                | None -> ()),
            [||]
        )

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
            router.onUrlChanged
                (fun newSegments ->
                    logger.Debug
                        (fun () -> $"RouterObserver. onUrlChanged. {JS.JSON.stringify {| newSegments = newSegments |}}"))
            router.children [
                yield! children
            ]
        ]

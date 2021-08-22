namespace FsUi.Hooks

open Browser.Types
open Feliz
open FsStore
open FsStore.Bindings
open FsStore.Hooks
open FsStore.Model
open FsUi.Bindings
open Fable.Core
open Feliz.UseListener
open FsJs


module Listener =
    let inline useKeyPress keys (fn: GetFn -> SetFn -> KeyboardEvent -> JS.Promise<unit>) =
        Profiling.addTimestamp "useKeyPress.render"

        let keyEvent = Store.useCallbackRef fn

        Rooks.useKey
            keys
            (keyEvent >> Promise.start)
            {|
                eventTypes =
                    [|
                        "keydown"
                        "keyup"
                    |]
            |}


    module State =
        let refHoveredAtomFamily<'T> = Store.atomFamily (fun (_elemRef: IRefValue<_>) ->
            printfn "refHoveredAtomFamily. returning defatul"
            false)

        let setElemRefHovered<'T> =
            Store.rawSetSelector
                (fun _getter setter (elemRef, newValue) ->
                    printfn $"setElemRefHovered. invoking. elemRef={elemRef} newValue={newValue}"
                    Store.set setter (refHoveredAtomFamily elemRef) newValue)

    let inline useElementHoverAtom (elemRef: IRefValue<_>) =
        //        let refHovered = Store.useState (State.refHoveredAtomFamily elemRef)


        let setIsHovered = Store.useSetState State.setElemRefHovered

        printfn $"useElementHoverAtom. elemRef={elemRef.current} setIsHovered={setIsHovered}"

        //        let isHovered, setIsHovered = React.useState false

        //        let setIsHoveredTrue =
//            React.useCallback (
//                (fun _ -> setIsHovered true),
//                [|
//                    box setIsHovered
//                |]
//            )
//
//        let setIsHoveredFalse =
//            React.useCallback (
//                (fun _ -> setIsHovered false),
//                [|
//                    box setIsHovered
//                |]
//            )

        React.useElementListener.onMouseEnter (elemRef, (fun _ ->
            printfn $"mouse enter triggered"
            setIsHovered (elemRef, true)))
        React.useElementListener.onMouseLeave (elemRef, (fun _ ->
            printfn $"mouse leave triggered"
            setIsHovered (elemRef, false)))

        State.refHoveredAtomFamily elemRef

    let inline useElementHover (elemRef: IRefValue<_>) =
//        let elementHoverAtom = useElementHoverAtom elemRef
//        let elementHover = Store.useValue elementHoverAtom
//        elementHover
        let isHovered, setIsHovered = React.useState false

        let setIsHoveredTrue =
            React.useCallback (
                (fun _ -> setIsHovered true),
                [|
                    box setIsHovered
                |]
            )

        let setIsHoveredFalse =
            React.useCallback (
                (fun _ -> setIsHovered false),
                [|
                    box setIsHovered
                |]
            )


        React.useElementListener.onMouseEnter (elemRef, setIsHoveredTrue)
        React.useElementListener.onMouseLeave (elemRef, setIsHoveredFalse)

        isHovered

//    let inline useElementHover (elemRef: IRefValue<#HTMLElement option>) =
//        let isHovered, setIsHovered = React.useState false
//
//        let setIsHoveredTrue =
//            React.useCallback (
//                (fun _ -> setIsHovered true),
//                [|
//                    box setIsHovered
//                |]
//            )
//
//        let setIsHoveredFalse =
//            React.useCallback (
//                (fun _ -> setIsHovered false),
//                [|
//                    box setIsHovered
//                |]
//            )
//
//        React.useElementListener.onMouseEnter (elemRef, setIsHoveredTrue)
//        React.useElementListener.onMouseLeave (elemRef, setIsHoveredFalse)
//
//        isHovered

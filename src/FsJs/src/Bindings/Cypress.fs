namespace FsJs.Bindings

open Browser.Types
open Fable.Core
open Fable.Core.JsInterop


module Cypress =
    let cypressTimeout = 200000

    let inline describe (title: string) (fn: unit -> unit) =
        emitJsExpr (title, fn) "describe($0, $1)"

    let inline before (fn: unit -> unit) = emitJsExpr fn "before($0)"

    type ExpectTo<'T> =
        abstract contain : 'T -> unit

    type Expect<'T> =
        abstract ``to`` : ExpectTo<'T>

    let inline expect<'T> (name: 'T) : Expect<'T> = emitJsExpr name "expect($0)"

    let inline it (name: string) (fn: unit -> unit) = emitJsExpr (name, fn) "it($0, $1)"

    module Cy =
        type Chainable<'T> =
            abstract should : ('T -> unit) -> unit

        type Chainable2<'T> =
            abstract should : string -> string -> string -> unit
            abstract invoke : string -> string -> string -> Chainable2<'T>
            abstract click : {| force: bool |} option -> Chainable2<'T>
            abstract contains : string -> {| timeout: int |} option -> Chainable2<'T>
            abstract debug : unit -> unit
            abstract clear : {| force: bool |} -> Chainable2<'T>
            abstract eq : int -> Chainable2<'T>
            abstract focus : unit -> unit
            abstract first : unit -> Chainable2<'T>
            abstract ``then`` : (Chainable2<'T> -> unit) -> unit
            abstract ``type`` : string -> {| force: bool |} -> Chainable2<'T>
            abstract scrollTo : string -> {| ensureScrollable: bool |} -> unit
            abstract get : string -> Chainable2<'T>
            abstract parents : string -> Chainable2<'T>
            abstract find : string -> Chainable2<'T>
            abstract children : string -> Chainable2<'T>

        type Location =
            abstract pathname : string
            abstract href : string
            abstract hash : string

        let inline location () : Chainable<Location> = emitJsExpr () "cy.location()"
        let inline wrap<'T> (el: Chainable2<'T>) : Chainable2<'T> = emitJsExpr el "cy.wrap($0)"
        let inline focused () : Chainable2<unit> = emitJsExpr () "cy.focused()"
        let inline visit (url: string) : unit = emitJsExpr url "cy.visit($0)"
        let inline should (fn: unit -> unit) : unit = emitJsExpr fn "cy.should($0)"
        let inline pause () : unit = emitJsExpr () "cy.pause()"
        let inline expect (obj: obj) : obj = emitJsExpr obj "expect($0)"
        let inline wait (time: int) : unit = emitJsExpr time "cy.wait($0)"
        let inline window () : JS.Promise<Window> = emitJsExpr () "cy.window()"
        let sinon: obj = emitJsExpr () "Cypress.sinon"

        let inline contains (text: string) (options: {| timeout: int |} option) : Chainable2<'T> =
            emitJsExpr (text, options) "cy.contains($0, $1)"

        let inline get (selector: string) : Chainable2<string> = emitJsExpr selector "cy.get($0)"


    module Cy2 =
        let typeText (fn: unit -> Cy.Chainable2<_>) (text: string) =
            Cy.wait 200
            fn().clear {| force = false |} |> ignore
            fn().should "be.empty" null null

            text
            |> Seq.iter
                (fun letter ->
                    Cy.wait 50

                    fn().first().click (Some {| force = false |})
                    |> ignore

                    fn().first().``type`` (string letter) {| force = false |}
                    |> ignore

                    Cy.wait 50)

            fn().should "have.value" text null

        let waitFocus selector wait =
            //            Cy.wait 50
            Cy.get(selector).should "have.focus" |> ignore

            match wait with
            | Some ms -> Cy.wait ms
            | None -> ()

        let selectorTypeText selector text wait =
            waitFocus selector wait
            typeText (fun () -> Cy.get selector) text

        let selectorFocusTypeText selector text =
            Cy.get(selector).first().focus ()
            typeText (fun () -> Cy.get selector) text

        let clickTestId selector =
            Cy
                .get(selector)
                .first()
                .click (Some {| force = false |})
            |> ignore

        let selectorFocusTypeTextWithinSelector parent selector text =
            Cy.get(parent).get(selector).first().focus ()
            typeText (fun () -> Cy.get(parent).get selector) text

        let clickTextWithinSelector selector text =
            (Cy.get(selector).contains text None)
                .click (Some {| force = false |})
            |> ignore

        let clickText text =
            (Cy.contains text None)
                .click (Some {| force = false |})
            |> ignore

        let clickSelectorChildFromText text selector =
            (Cy.contains text None)
                .find(selector)
                .click (Some {| force = false |})
            |> ignore

        let clickSelector selector =
            (Cy.get selector).first().click None |> ignore

        let waitForWithinSelector selector text options =
            (Cy.get(selector).contains text options)
                .should "be.visible"
            |> ignore

        let waitFor text options =
            (Cy.contains text options).should "be.visible"
            |> ignore

        let expectLocation expected =
            Cy
                .location()
                .should (fun location -> expect(location.href).``to``.contain expected)

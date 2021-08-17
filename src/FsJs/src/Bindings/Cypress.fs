namespace FsJs.Bindings

open Browser.Types
open Fable.Core
open Fable.Core.JsInterop


module Cypress =
    let cypressTimeout = 200000

    let inline describe (title: string) (fn: unit -> unit) =
        emitJsExpr (title, fn) "describe($0, $1)"

    let inline before (fn: unit -> unit) = emitJsExpr fn "before($0)"

    type ExpectToBe<'T> =
        abstract equal : obj -> unit

    type ExpectTo<'T> =
        abstract contain : obj -> unit
        abstract be : ExpectToBe<'T>

    type Expect<'T> =
        abstract ``to`` : ExpectTo<'T>

    let inline expect<'T> (sel: obj) : Expect<'T> = emitJsExpr sel "expect($0)"

    let inline it (name: string) (fn: unit -> unit) = emitJsExpr (name, fn) "it($0, $1)"

    module Cy =
        type Chainable<'T> =
            abstract should : ('T -> unit) -> unit

        type Chainable2<'T> =
            abstract should : string -> unit
            abstract should : string * string -> unit
            abstract should : string * string * string -> unit
            abstract invoke : string -> JS.Promise<obj>
            abstract invoke : string * string -> JS.Promise<obj>
            abstract invoke : string * string * string -> JS.Promise<obj>
            abstract click : {| force: bool |} option -> Chainable2<'T>
            abstract contains : string -> {| timeout: int |} option -> Chainable2<'T>
            abstract debug : unit -> unit
            abstract clear : {| force: bool |} -> Chainable2<'T>
            abstract eq : int -> Chainable2<'T>
            abstract its : string -> {| timeout: int |} option -> Chainable2<'T>
            abstract focus : unit -> unit
            abstract first : unit -> Chainable2<'T>
            abstract ``then`` : (Chainable2<'T> -> unit) -> unit

            abstract ``type`` :
                string ->
                {| force: bool
                   parseSpecialCharSequences: bool |} ->
                Chainable2<'T>

            abstract scrollTo : string -> {| ensureScrollable: bool |} -> unit
            abstract get : string -> Chainable2<'T>
            //            abstract get : string -> Async<_>
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
        let inline wait (time: int) : unit = emitJsExpr time "cy.wait($0)"
        let inline window () : JS.Promise<Window> = emitJsExpr () "cy.window()"

        let inline contains (text: string) (options: {| timeout: int |} option) : Chainable2<'T> =
            emitJsExpr (text, options) "cy.contains($0, $1)"

        let sinon: obj = emitJsExpr () "Cypress.sinon"

        let inline cypressPromise<'T> (fn: ('T -> unit) -> (string -> unit) -> unit) : JS.Promise<'T> =
            emitJsExpr fn "new Cypress.Promise((res, err) => { $0(res, err) })"


        //        function waitOneSecond() {
//    // return a promise that resolves after 1 second
//    return new Cypress.Promise((resolve, reject) => {
//      setTimeout(() => {
//        // set waited to true
//        waited = true
//
//        // resolve with 'foo' string
//        resolve('foo')
//      }, 1000)
//    })
//  }

        let inline elContains
            (el: Chainable2<'T>)
            (text: string)
            (options: {| timeout: int |} option)
            : Chainable2<'T> =
            emitJsExpr (el, text, options) "$0.contains($1, $2)"

        let inline get (selector: string) (options: {| timeout: int |} option) : Chainable2<'T> =
            emitJsExpr (selector, options) "cy.get($0, $1)"

        let defaultOptions = Some {| timeout = cypressTimeout |}


    module Cy2 =
        let typeText (fn: unit -> Cy.Chainable2<_>) (text: string) =
            Cy.wait 200
            fn().clear {| force = false |} |> ignore
            fn().should "be.empty"

            text
            |> Seq.iter
                (fun letter ->
                    Cy.wait 50

                    fn().first().click (Some {| force = false |})
                    |> ignore

                    fn().first().``type``
                        (string letter)
                        {|
                            force = false
                            parseSpecialCharSequences = true
                        |}
                    |> ignore

                    Cy.wait 50)

            fn().should ("have.value", text)


        let inline waitFocus selector wait =
            //            Cy.wait 50
            (Cy.get selector Cy.defaultOptions)
                .should "have.focus"

            match wait with
            | Some ms -> Cy.wait ms
            | None -> ()

        let inline selectorTypeText selector text wait =
            waitFocus selector wait
            typeText (fun () -> Cy.get selector Cy.defaultOptions) text

        let inline selectorFocusTypeText selector text =
            (Cy.get selector Cy.defaultOptions)
                .first()
                .focus ()

            typeText (fun () -> Cy.get selector Cy.defaultOptions) text

        let inline clickTestId selector =
            (Cy.get selector Cy.defaultOptions)
                .first()
                .click (Some {| force = false |})
            |> ignore

        let inline selectorFocusTypeTextWithinSelector parent selector text =
            (Cy.get parent Cy.defaultOptions)
                .get(selector)
                .first()
                .focus ()

            typeText (fun () -> (Cy.get parent Cy.defaultOptions).get selector) text

        let inline clickTextWithinSelector selector text =
            let contains = (Cy.get selector Cy.defaultOptions).contains text None

            contains.click (Some {| force = false |})
            |> ignore

        let inline clickEl (el: Cy.Chainable2<_>) =
            el.click (Some {| force = false |}) |> ignore


        let inline find selector (el: Cy.Chainable2<_>) = el.find selector // |> Cy.wrap
        //            Promise.create
//                (fun res err ->
//                    try
//                        el.find selector |> Cy.wrap |> res
//                    with
//                    | ex ->
//                        printfn $"find error: {ex}"
//                        err ex)

        let inline textContains text = Cy.contains text Cy.defaultOptions

        let inline clickText text =
            let contains = textContains text
            clickEl contains

        let inline textEl el text =
            (Cy.elContains el text Cy.defaultOptions)

        let inline clickTextEl el text = clickEl (textEl el text)

        let inline clickSelectorChildFromText text selector =
            let contains = textContains text
            clickEl (contains.find selector)

        let inline clickSelector selector =
            (Cy.get selector Cy.defaultOptions)
                .first()
                .click None
            |> ignore

        let inline shouldBeVisible (el: Cy.Chainable2<_>) = el.should "be.visible"

        let inline waitForWithinSelector selector text options =
            let contains = (Cy.get selector Cy.defaultOptions).contains text options
            contains |> shouldBeVisible

        let inline waitForOptions text options =
            let contains = Cy.contains text options
            contains |> shouldBeVisible

        let inline waitForTimeout text timeout =
            waitForOptions text (Some {| timeout = timeout |})

        let homeUrl = "https://localhost:33922"
        let inline waitFor text = waitForTimeout text cypressTimeout

        let inline waitForEl el text = textEl el text |> shouldBeVisible

        //        let inline waitForElText (el: Cy.Chainable2<_>) fn =
//            (el.invoke "text")?``then`` fn

        let inline expectLocation expected =
            Cy
                .location()
                .should (fun location -> expect(location.href).``to``.contain expected)

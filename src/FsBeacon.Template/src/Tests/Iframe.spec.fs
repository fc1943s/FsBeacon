namespace FsBeacon.Template.Tests

open Fable.Core
open FsCore
open Fable.Core.JsInterop
open Fable.Extras
open FsJs
open FsJs.Bindings.Cypress
open FsStore.Bindings


module Iframe =
    //    let toCypressPromise p =
//        promise {
//            let! value = p
//            return value
////            return! Cy.cypressPromise (fun res _err -> res value)
//        }


    let inline waitForElSelectorObjectKey<'T> (elFn: unit -> Cy.Chainable2<obj>) selector key nextKey =
        Cy2.waitForEl (elFn ()) $"\"{key}\": {{"
        let el2 = (elFn ()).find selector
        let textPromise = el2.invoke "text"

        textPromise
        |> Promise.map
            (fun text ->
                let regex = JSe.RegExp @$"\""{key}\"": *(.*?), *\""{nextKey}\"""

                let textFmt =
                    text
                    |> string
                    |> String.replace "\r" ""
                    |> String.replace "\n" ""

                let matches =
                    textFmt
                    |> regex.Match
                    |> Option.ofObj
                    |> Option.defaultValue Seq.empty
                    |> Seq.toList

                match matches with
                | _ :: x :: _ -> x |> Json.decode<'T> |> Some
                | _ -> None)

    let typeText (el: Cy.Chainable2<_>) (text: string) =
        el.invoke ("val", text |> String.substring 0 (text.Length - 1))
        |> ignore

        el.``type``
            (text |> String.substringFrom -1)
            {|
                force = false
                parseSpecialCharSequences = false
            |}
        |> ignore

    describe
        "tests"
        (fun () ->
            let homeUrl = "https://localhost:9762"
            before (fun () -> Cy.visit homeUrl)

            it
                "login"
                (fun () ->
                    Cy.window ()
                    |> Promise.iter (fun window -> window?indexedDB?deleteDatabase "radata")

                    Cy2.expectLocation $"{homeUrl}/"

                    Cy2.waitFor $""""<0> registerAtom() FsStore/logLevel atom3 AtomWithStorage": "1","""

                    waitForElSelectorObjectKey<Gun.GunKeys> Cy.getIframeBody1 "#component" "Keys" "SessionRestored"
                    |> Promise.bind
                        (fun keys ->
                            promise {
                                let keysJson = JS.JSON.stringify keys
                                Dom.Logger.getLogger().Warning (fun () -> $"test: keys1={keysJson}")

                                typeText (Cy.getIframeBody2().find "#keys") keysJson
                            })
                    |> Promise.iter id

                    waitForElSelectorObjectKey<Gun.GunKeys> Cy.getIframeBody2 "#component" "Keys" "SessionRestored"
                    |> Promise.bind
                        (fun keys ->
                            promise {
                                let keysJson = JS.JSON.stringify keys
                                Dom.Logger.Default.Warning (fun () -> $"test: keys2={keysJson}")
                            })
                    |> Promise.iter id

                    Cy2.waitForEl (Cy.getIframeBody2 ()) "\"Alias\": \""

                    //                    Cy2.waitForEl (get1 ()) "\"<44> FsUi/systemUiFont get\": \"2\""

                    //                    Cy2.waitForEl (get1 ()) $"""49212:file[0]=0%%"""
//                    Cy2.waitForEl (get1 ()) $"""49212:file[1]=0%%"""
//                    Cy2.waitForEl (get2 ()) $"""49222:file[0]=0%%"""
//                    Cy2.waitForEl (get2 ()) $"""49222:file[1]=0%%"""
//
//                    Cy2.clickTextEl (get1 ()) $"""49212:file[0]:save"""
//
//                    Cy2.waitForEl (get1 ()) $"""49212:file[0]=100%%"""
//                    Cy2.waitForEl (get2 ()) $"""49222:file[0]=100%%"""
//
//                    Cy2.clickTextEl (get2 ()) $"""49222:file[1]:save"""
//
//                    Cy2.waitForEl (get1 ()) $"""49212:file[1]=100%%"""
//                    Cy2.waitForEl (get2 ()) $"""49222:file[1]=100%%"""

                    //// cypress/integration/custom-command-spec.js
//it('gets the post using custom command', () => {
//  cy.visit('index.html')
//  cy.getIframeBody()
//    .find('#run-button').should('have.text', 'Try it').click()
//  cy.getIframeBody()
//    .find('#result').should('include.text', '"delectus aut autem"')
//})


                    Cy.window ()
                    |> Promise.iter
                        (fun window ->
                            (expect ((window?_global?get "profilingState")?CallCount?get "App().render"))
                                .``to``.be.equal 2)



                    //let x = $""""registerAtom FsStore/logLevel": "1""""
                    ()
                    //

                    //                    Cy
//                        .get("@consoleLog")
//                        .should (
//                            unbox
//                                (fun x ->
//                                    let calls: string [] [] = x?getCalls ()
//                                    let args = calls |> Array.map (String.concat " ")
//                                    Dom.consoleLog( [|$"calls={calls} args={args} x={x}"|])
//                                    failwith x
//
//                                    args
//                                    |> Array.iter (fun y -> ((Cy.expect y)?``to``?deep?equal) x))
//                        ) |> ignore

                    //                    Cy.get("@consoleLog").should
//                        "have.been.calledWith"
//                        (Cy.sinon?``match``)
//                        "registerAtom FsStore/logLevel"

                    //                    Cy.get("@consoleLog").should "be.calledWithMatch" "*registerAtom FsStore/logLevel" null
//                    Cy.should
//                        (fun () ->
//                            Cy.window ()
//                            |> Promise.iter
//                                (fun window -> (Cy.expect (window?lastConsoleLogArgs))?``to``?equal "Hello World!"))
                    //
//                    Cy.get("@consoleError").should "be.calledWithMatch" "/React 18/" null
//
//                    Cy.get("@consoleLog").should "be.calledWithMatch" "DebugPanel.render. showDebug=false" null



                    //                    Cy.window ()
//                    |> Promise.iter (fun window -> window?Debug <- false)
                    ))

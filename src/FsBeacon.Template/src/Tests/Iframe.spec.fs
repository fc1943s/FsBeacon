namespace FsBeacon.Template.Tests

open Fable.Core.JsInterop
open FsJs
open FsJs.Bindings.Cypress


module Iframe =
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

                    let get1 () = Cy.getIframeBody1 ()
                    let get2 () = Cy.getIframeBody2 ()


                    Cy2.waitForEl (get1 ()) "\"<44> FsUi/systemUiFont get\": \"2\""

                    Cy2.waitForEl (get1 ()) $"""49212:file[0]=0%%"""
                    Cy2.waitForEl (get1 ()) $"""49212:file[1]=0%%"""
                    Cy2.waitForEl (get2 ()) $"""49222:file[0]=0%%"""
                    Cy2.waitForEl (get2 ()) $"""49222:file[1]=0%%"""

                    Cy2.clickTextEl (get1 ()) $"""49212:file[0]:save"""

                    Cy2.waitForEl (get1 ()) $"""49212:file[0]=100%%"""
                    Cy2.waitForEl (get2 ()) $"""49222:file[0]=100%%"""

                    Cy2.clickTextEl (get2 ()) $"""49222:file[1]:save"""

                    Cy2.waitForEl (get1 ()) $"""49212:file[1]=100%%"""
                    Cy2.waitForEl (get2 ()) $"""49222:file[1]=100%%"""

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

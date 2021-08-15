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

                    Cy2.waitFor "Wrong user or password"

                    let x = $""""registerAtom FsStore/logLevel": "1""""
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

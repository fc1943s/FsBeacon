namespace FsBeacon.Template.Tests

open Fable.Core.JsInterop
open FsJs.Bindings.Cypress


module Iframe =
    describe
        "tests"
        (fun () ->
            let homeUrl = "https://localhost:33922"
            before (fun () -> Cy.visit homeUrl)

            it
                "login"
                (fun () ->
                    Cy.window ()
                    |> Promise.iter (fun window -> window?indexedDB?deleteDatabase "radata")

                    Cy2.expectLocation $"{homeUrl}/"
                    Cy.get("body").should "have.css" "background-color" "rgb(222, 222, 222)"

                    Cy.focused().click None |> ignore

                    Cy.window ()
                    |> Promise.iter (fun window -> window?Debug <- false)

                    Cy.visit homeUrl))

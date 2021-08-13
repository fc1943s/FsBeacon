namespace FsBeacon.Template.Tests

open Fable.Core.JsInterop
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

                    Cy.window ()
                    |> Promise.iter (fun window -> window?Debug <- false)

                    Cy.visit homeUrl))

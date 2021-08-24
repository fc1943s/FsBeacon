namespace FsBeacon.Template.Tests

open Fable.Core.JsInterop
open Fable.Jester
open FsJs
open FsJs.Bindings
open FsJs.Bindings.Cypress
open Fable.Jester
open Fable.ReactTestingLibrary
open Fable.Core.JsInterop
open FsJs
open FsJs.Dom
open Microsoft.FSharp.Core.Operators

module Sync =

    describe
        "sync"
        (fun () ->
            let homeUrl = "https://localhost:49212"

            before (fun () -> Cy.visit homeUrl)

//            after (fun () -> globalExit.Write globalSet true)

            it
                "all"
                (fun () ->
                    Cy.window ()
                    |> Promise.iter (fun window -> window?indexedDB?deleteDatabase "radata")

                    Cy2.expectLocation $"{homeUrl}/"

                    // atom
                    // atomFamily
                    // atomWithStorage
                    // atomWithSync
                    // atomFamilyWithSync
                    // atomWithStorageSync


                    //                    Cy2.clickText "clear logs"

//                    Cy2.clickText "mount"
//                    Cy2.clickText "hydrate"
//                    Cy2.clickText "clear logs"


//                    Cy2.clickText "sign in"






                    //                    Cy2.waitFor "<30> @@"
//                    Cy2.clickText "unmount"
//                    Cy2.waitFor "<34>"
//                    Cy2.clickText "mount"
//                    Cy2.waitFor "<37>"
//                    Cy2.clickText "unmount"
//                    Cy2.waitFor "<38>"
//                    Cy2.clickText "mount"
//                    Cy2.waitFor "<41>"
//                    Cy2.clickText "unmount"
//                    Cy2.waitFor "<42>"
//                    Cy2.clickText "mount"
//                    Cy2.waitFor "<45>"
                    //                    Cy2.clickText "unmount"
//                    Cy2.clickText "mount"
                    ))

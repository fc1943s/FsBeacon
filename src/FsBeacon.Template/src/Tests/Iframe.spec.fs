namespace FsBeacon.Template.Tests

open Fable.Core
open FsCore
open Fable.Core.JsInterop
open Fable.Extras
open FsJs
open FsJs.Bindings.Cypress
open FsStore
open FsStore.Bindings
open FsStore.Model


module Iframe =
    let inline waitForElSelectorValueIndicator<'T> (elFn: unit -> Cy.Chainable2<obj>) selector key =
        Cy2.waitForEl (elFn ()) $"[{key}={{"
        let el2 = (elFn ()).find selector
        let textPromise = el2.invoke "text"

        textPromise
        |> Promise.map
            (fun text ->
                let regex = JSe.RegExp $"\[{key}=(.*?)\]\["

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

    //    let inline actorSignIn elFn fn =
//        waitForElSelectorObjectKey<Gun.GunKeys> elFn "#component" "PrivateKeys" "SessionRestored"
//        |> Promise.bind
//            (fun privateKeys ->
//                promise {
//                    let keysJson = JS.JSON.stringify privateKeys
//                    do! fn keysJson
//                })
//        |> Promise.iter id


    //    let typeText (el: Cy.Chainable2<_>) (text: string) =
//        el.invoke ("val", text |> String.substring 0 (text.Length - 1))
//        |> ignore
//
//        el.``type``
//            (text |> String.substringFrom -1)
//            {|
//                force = false
//                parseSpecialCharSequences = false
//            |}
//        |> ignore

    let inline someFn fn p elFnList =
        elFnList |> List.iter (fun elFn -> fn (elFn ()) p)

    let inline getIframeN n =
        (Cy.get $"""iframe[data-cy="iframe{n}"]""" Cy.defaultOptions)

    let inline getIframeBodyN n =
        (getIframeN n).its "0.contentDocument.body" Cy.defaultOptions

    let inline get1 () = getIframeBodyN 1
    let inline get2 () = getIframeBodyN 2
    let inline get3 () = getIframeBodyN 3

    let iframes =
        [
            get1
            get2
            get3
        ]

    let inline allFn fn p = iframes |> someFn fn p

    describe
        "iframe"
        (fun () ->
            let homeUrl = "https://localhost:9762"

            before (fun () -> Cy.visit homeUrl)

            it
                "all"
                (fun () ->
                    Cy.window ()
                    |> Promise.iter (fun window -> window?indexedDB?deleteDatabase "radata")

                    Cy2.expectLocation $"{homeUrl}/"

                    Cy2.waitFor "\"IsTesting\": true,"

                    allFn (fun node () -> node.should "not.be.empty") ()

                    allFn Cy2.waitForEl "async alias: undefined"
                    allFn Cy2.clickTextEl "hydrate"

                    [
                        get1
                        get2
                    ]
                    |> someFn Cy2.clickTextEl "sign in"

                    waitForElSelectorValueIndicator<Gun.GunKeys> get2 "#component" "privateKeys"
                    |> Promise.bind
                        (fun privateKeys ->
                            promise {
                                let keysJson = JS.JSON.stringify privateKeys

                                let keys = keysJson |> Json.decode<Gun.GunKeys>
                                let message = Message.Command (AppCommand.SignInPair keys)
                                let messages = message |> Array.singleton

                                let json =
                                    messages
                                    |> Json.encode<Message<AppCommand, AppEvent> []>

                                let base64 =
                                    match Dom.window () with
                                    | Some window -> window?btoa json
                                    | None -> ""

                                Logger.logWarning
                                    (fun () ->
                                        $"test: keys get2 waitForElSelectorObjectKey. json={json} base64={base64}")

                                if base64 <> "" then
                                    let! _ =
                                        (getIframeN 3)
                                            .invoke ("attr", "src", $"https://localhost:49222/#{base64}")

                                    ()

                                ()
                            })
                    |> Promise.iter id

                    //                    actorSignIn get2 (fun keysJson -> promise { Dom.logWarning (fun () -> $"test: keys actorsignin get2 ={keysJson}") })

                    allFn Cy2.waitForEl "async alias: a@"

                    Cy2.clickTextEl (get2 ()) "add file"

                    //                    Cy2.waitForEl (Cy.getIframeBody1 ()) "async alias: undefined"
//                    Cy2.waitForEl (Cy.getIframeBody2 ()) "async alias: undefined"
//                    Cy2.waitForEl (Cy.getIframeBody3 ()) "async alias: undefined"
//
//                    Cy2.clickTextEl (Cy.getIframeBody1 ()) "hydrate"
//                    Cy2.clickTextEl (Cy.getIframeBody1 ()) "hydrate"

                    //                    actorSignIn
//                        Cy.getIframeBody1
//                        (fun keysJson ->
//                            promise {
//                                Dom
//                                    .logWarning (fun () -> $"test: keys1={keysJson}")
//
//                                typeText (Cy.getIframeBody3().find "#privateKeys") keysJson
//
//                            })
//
//                    actorSignIn
//                        Cy.getIframeBody2
//                        (fun keysJson ->
//                            promise {
//                                Dom
//                                    .logWarning (fun () -> $"test: keys2={keysJson}")
//
//                            })
//
//                    Cy2.waitForEl (Cy.getIframeBody1 ()) "async alias: a@"
//                    Cy2.waitForEl (Cy.getIframeBody2 ()) "async alias: a@"
//                    Cy2.waitForEl (Cy.getIframeBody3 ()) "async alias: a@"

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




                    promise {
                        let! profilingState = Profiling.globalProfilingState.AsyncRead globalGet

                        expect(profilingState.CountMap.["App [ render ] "])
                            .``to``.be.equal 2

                    }
                    |> Promise.iter id


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

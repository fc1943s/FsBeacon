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

            before
                (fun () ->

                    Cy.visit homeUrl)

            it
                "all"
                (fun () ->
                    Cy.window ()
                    |> Promise.iter (fun window -> window?indexedDB?deleteDatabase "radata")

                    Cy2.expectLocation $"{homeUrl}/"

                    Cy2.waitFor "\"IsTesting\": true,"

                    Cy2.waitFor
                        "160. FsStore | Engine.createAtomWithSubscription [ debouncedSync ](a2) | localAdaptersAtom=atom54 atomType=Data,System.Boolean atomPath=FsUi/systemUiFont defaultValue=true"


                    allFn (fun node () -> node.should "not.be.empty") ()

                    allFn Cy2.waitForEl "alias=null"
                    allFn Cy2.clickTextEl "mount"
                    allFn Cy2.clickTextEl "hydrate"

                    [
                        get1
                        get2
                    ]
                    |> someFn Cy2.clickTextEl "sign in"

                    allFn Cy2.clickTextEl "clear logs"

                    waitForElSelectorValueIndicator<Gun.GunKeys> get2 "#debug" "privateKeys"
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

                    allFn Cy2.clickTextEl "clear logs"
                    allFn Cy2.clickTextEl "disable logs"

                    Cy2.waitForEl (get3 ()) "logout (alias@"

                    let fileCount = 5

                    for i = 1 to fileCount do
                        Cy2.clickTextEl (get2 ()) "add file"
                        Cy2.clickTextEl (get3 ()) "add file"

                    for i = fileCount * 2 downto 1 do
                        Cy2.waitForEl (get2 ()) $"index={i} progress=100%%"
                        Cy2.waitForEl (get3 ()) $"index={i} progress=100%%"

//                    for i = fileCount * 2 downto 1 do
//                        Cy2.clickTextEl (get2 ()) $"[{i}]:delete"
//
//                    Cy2.waitForEl (get3 ()) "file count: 0"
                    ))

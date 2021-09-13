namespace FsBeacon.Template.Tests

open System
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
                        "164. FsStore | Engine.createAtomWithSubscription [ debouncedSync ](a2) localAdaptersAtom=atom58 atomType=Data,System.Boolean atomPath=FsUi/systemUiFont defaultValue=true"


                    allFn (fun node () -> node.should "not.be.empty") ()

                    allFn Cy2.waitForEl "alias=null"

                    allFn Cy2.clickTextEl "mount"

                    Cy2.clickTextEl (get2 ()) "disable logs"
                    Cy2.clickTextEl (get3 ()) "disable logs"

                    allFn Cy2.clickTextEl "disable sync"

                    Cy2.clickTextEl (get1 ()) "enable gun sync"

                    Cy2.clickTextEl (get2 ()) "enable gun sync"
                    Cy2.clickTextEl (get2 ()) "enable hub sync"

                    Cy2.clickTextEl (get3 ()) "enable hub sync"

                    Cy2.clickTextEl (get1 ()) "sign in"
                    Cy2.clickTextEl (get3 ()) "sign in"

                    allFn Cy2.clickTextEl "clear logs"

                    Cy2.clickTextEl (get2 ()) "enable logs"

                    waitForElSelectorValueIndicator<Gun.GunKeys> get1 "#debug" "privateKeys"
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

                                let getLocals () =
                                    $"json={json} base64={base64} {getLocals ()}"

                                Logger.logWarning (fun () -> "test: keys get1 waitForElSelectorObjectKey") getLocals

                                if base64 <> "" then
                                    let! _ =
                                        (getIframeN 2)
                                            .invoke ("attr", "src", $"https://localhost:49222/#{base64}")

                                    ()

                                ()
                            })
                    |> Promise.iter id

//                    // TODO: remove
//                    Cy2.clickTextEl (get2 ()) "sign in"

                    Cy2.clickTextEl (get1 ()) "disable logs"

                    Cy2.waitForEl (get3 ()) "logout (alias@"

                    waitForElSelectorValueIndicator<Gun.GunKeys> get2 "#debug" "privateKeys"
                    |> Promise.bind
                        (fun privateKeys ->
                            promise {
                                let getLocals () =
                                    $"privateKeys={privateKeys} {getLocals ()}"

                                Logger.logWarning (fun () -> "test: keys get2 waitForElSelectorObjectKey") getLocals
                            })
                    |> Promise.iter id

                    let fileCount = 2

                    Cy2.clickTextEl (get2 ()) "disable logs"

                    Cy2.clickTextEl (get2 ()) "reset counter"
                    Cy2.clickTextEl (get2 ()) "counter (+0)"

                    for i = 1 to fileCount do
                        Cy2.clickTextEl (get1 ()) "add file"
                        Cy2.clickTextEl (get3 ()) "add file"

                    for i = 1 to fileCount * 2 do
                        allFn Cy2.waitForEl $"index={i} progress=100%%"

                    for i = fileCount * 2 downto 1 do
                        Cy2.clickTextEl (get2 ()) $"[{i}]:delete"

                    allFn Cy2.waitForEl "file count: 0"
                    Cy2.waitForEl (get1 ()) "counter (+1)"
                    Cy2.waitForEl (get3 ()) "counter (+1)"

                    ))

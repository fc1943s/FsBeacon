namespace FsBeacon.Template.Tests

open Fable.Core.JsInterop
open Fable.Extras
open FsJs
open FsJs.Bindings.Cypress

module Cy2 =
    let waitForRegex (regex: string) =
        Cy2.waitFor (regex |> JSe.RegExp |> unbox)

    let shouldNotBeVisible (el: Cy.Chainable2<_>) = el.should "not.be.visible"


module Files =
    describe
        "files"
        (fun () ->
            let homeUrl = "https://localhost:49212"

            before (fun () -> Cy.visit homeUrl)

            it
                "all"
                (fun () ->
                    Cy.window ()
                    |> Promise.iter (fun window -> window?indexedDB?deleteDatabase "radata")

                    Cy2.expectLocation $"{homeUrl}/"

                    Cy2.waitFor "\"IsTesting\": true,"
                    Cy2.waitFor "\"IsElectron\": false,"

                    Cy2.waitFor "\"Profiling body = "

                    // atom
                    // atomFamily
                    // atomWithStorage
                    // atomWithSync
                    // atomFamilyWithSync
                    // atomWithStorageSync


                    Cy2.waitForRegex """"<0> #Primitives.registerAtom FsStore/logLevel atom4 AtomWithStorage": "1","""
                    Cy2.waitForRegex """"<1> #Primitives.registerAtom FsStore/showDebug atom7 AtomWithStorage": "1","""

                    Cy2.waitForRegex
                        """"<2> #Primitives.registerAtom FsStore/gunOptions atom10 AtomWithStorage": "1","""

                    Cy2.waitForRegex """"<3> #Primitives.registerAtom FsStore/hubUrl atom13 AtomWithStorage": "1","""
                    Cy2.waitForRegex """"<4> #Primitives.atom defaultValue getter FsStore/gunTrigger 0": "1","""
                    Cy2.waitForRegex """"<5> #Primitives.registerAtom FsStore/gunTrigger atom14 Atom": "1","""
                    Cy2.waitForRegex """"<6> #Primitives.atom defaultValue getter FsStore/hubTrigger 0": "1","""
                    Cy2.waitForRegex """"<7> #Primitives.registerAtom FsStore/hubTrigger atom15 Atom": "1","""
                    Cy2.waitForRegex """"<8> #Primitives.atom defaultValue getter FsStore/routeTrigger 0": "1","""
                    Cy2.waitForRegex """"<9> #Primitives.registerAtom FsStore/routeTrigger atom16 Atom": "1","""

                    Cy2.waitForRegex
                        """"<10> #Primitives.atom defaultValue getter FsStore/sessionRestored false": "1","""

                    Cy2.waitForRegex """"<11> #Primitives.registerAtom FsStore/sessionRestored atom17 Atom": "1","""
                    Cy2.waitForRegex """"<12> #Primitives.registerAtom FsUi/darkMode atom36 AtomWithStorage": "1","""
                    Cy2.waitForRegex """"<13> #Primitives.registerAtom FsUi/darkMode atom37 AtomWithSync": "1","""

                    Cy2.waitForRegex
                        """"<14> #Primitives.registerAtom FsUi/darkMode atom40 AtomWithStorageSync": "1","""

                    Cy2.waitForRegex """"<15> #Primitives.registerAtom FsUi/fontSize atom43 AtomWithStorage": "1","""
                    Cy2.waitForRegex """"<16> #Primitives.registerAtom FsUi/fontSize atom44 AtomWithSync": "1","""

                    Cy2.waitForRegex
                        """"<17> #Primitives.registerAtom FsUi/fontSize atom47 AtomWithStorageSync": "1","""

                    Cy2.waitForRegex
                        """"<18> #Primitives.registerAtom FsUi/systemUiFont atom50 AtomWithStorage": "1","""

                    Cy2.waitForRegex """"<19> #Primitives.registerAtom FsUi/systemUiFont atom51 AtomWithSync": "1","""

                    Cy2.waitForRegex
                        """"<20> #Primitives.registerAtom FsUi/systemUiFont atom54 AtomWithStorageSync": "1","""

                    Cy2.waitForRegex
                        """"<21> #Primitives.atom defaultValue getter FsBeacon/syncHydrateStarted false": "1","""

                    Cy2.waitForRegex """"<22> #Primitives.registerAtom FsBeacon/syncHydrateStarted atom55 Atom": "1","""

                    Cy2.waitForRegex
                        """"<23> #Primitives.atom defaultValue getter FsBeacon/syncHydrateCompleted false": "1","""

                    Cy2.waitForRegex
                        """"<24> #Primitives.registerAtom FsBeacon/syncHydrateCompleted atom56 Atom": "1","""

                    Cy2.waitForRegex
                        """"<25> #Primitives.atom defaultValue getter FsBeacon/signInStarted false": "1","""

                    Cy2.waitForRegex """"<26> #Primitives.registerAtom FsBeacon/signInStarted atom57 Atom": "1","""
                    Cy2.waitForRegex """"<27> #Primitives.atom defaultValue getter FsBeacon/mounted false": "1","""
                    Cy2.waitForRegex """"<28> #Primitives.registerAtom FsBeacon/mounted atom58 Atom": "1","""

                    Cy2.waitForRegex
                        """"<29> #Primitives.registerAtom FsStore/File/00000000-0000-0000-0000-000000000000/pub atom59 AtomWithSync": "1","""

                    Cy2.waitForRegex
                        """"<30> #Primitives.registerAtom FsStore/Message/00000000-0000-0000-0000-000000000000/ack atom65 AtomWithSync": "1","""

                    Cy2.waitForRegex """"<31> App.render": "2","""
                    Cy2.waitForRegex """"<32> RootWrapper.render": "2","""
                    Cy2.waitForRegex """"<33> #Primitives.selector get FsStore/logger": "1","""
                    Cy2.waitForRegex """"<34> #Primitives.selector get FsStore/logLevel": "1","""
                    Cy2.waitForRegex """"<35> #Primitives.selector get FsStore/gun": "1","""
                    Cy2.waitForRegex """"<36> #Primitives.selector get FsStore/gunPeers": "1","""
                    Cy2.waitForRegex """"<37> #Primitives.selector get FsStore/gunOptions": "1","""
                    Cy2.waitForRegex """"<38> #Primitives.selector get FsStore/alias": "1","""
                    Cy2.waitForRegex """"<39> #Primitives.selector get FsStore/gunUser": "1","""
                    Cy2.waitForRegex """"<40> #Primitives.selector get FsStore/deviceInfo": "1","""
                    Cy2.waitForRegex """"<41> #Primitives.selector get FsUi/darkMode": "4","""
                    Cy2.waitForRegex """"<42> #Primitives.selector get FsStore/atomAccessors": "3","""
                    Cy2.waitForRegex """"<43> #Primitives.selector get FsStore/valueWrapper": "3","""
                    Cy2.waitForRegex """"<44> #Primitives.selector get FsStore/lastSyncValueByTypeAtom": "3","""
                    Cy2.waitForRegex """"<45> FsUi/darkMode get": "2","""
                    Cy2.waitForRegex """"<46> ThemeLoader.render": "2","""
                    Cy2.waitForRegex """"<47> #Primitives.selector set FsStore/valueWrapper": "2","""
                    Cy2.waitForRegex """"<48> #Primitives.selector get FsStore/privateKeys": "1","""
                    Cy2.waitForRegex """"<49> #Primitives.selector get FsStore/hubUrl": "1","""
                    Cy2.waitForRegex """"<50> #Primitives.selector get FsUi/uiState": "1","""
                    Cy2.waitForRegex """"<51> #Primitives.selector get FsUi/fontSize": "4","""
                    Cy2.waitForRegex """"<52> FsUi/fontSize get": "2","""
                    Cy2.waitForRegex """"<53> #Primitives.selector get FsUi/systemUiFont": "4","""
                    Cy2.waitForRegex """"<54> FsUi/systemUiFont get": "2","""
                    Cy2.waitForRegex """"<55> @FsUi/darkMode": "1","""
                    Cy2.waitForRegex """"<56> @FsUi/fontSize": "1","""
                    Cy2.waitForRegex """"<57> @FsUi/systemUiFont": "1"[^,]"""

                    Cy2.clickText "clear logs"
                    Cy2.clickText "mount"

                    Cy2.waitForRegex """"<0> #Primitives.selector get FsBeacon/asyncMessageIdAtoms": "1","""
                    Cy2.waitForRegex """"<1> #Primitives.selector get FsStore/asyncFileIdAtoms": "1","""
                    Cy2.waitForRegex """"<2> @FsUi/fontSize": "0","""
                    Cy2.waitForRegex """"<3> @FsUi/systemUiFont": "0","""
                    Cy2.waitForRegex """"<4> #Primitives.asyncSelector get FsStore/asyncAlias": "1"[^,]"""

                    Cy2.clickText "clear logs"
                    Cy2.clickText "hydrate"

                    Cy2.waitForRegex """"<0> #Primitives.selector get FsStore/gunPeers": "1","""
                    Cy2.waitForRegex """"<1> #Primitives.selector get FsStore/gun": "1","""
                    Cy2.waitForRegex """"<2> #Primitives.selector get FsStore/gunUser": "1","""
                    Cy2.waitForRegex """"<3> #Primitives.selector get FsStore/alias": "1","""
                    Cy2.waitForRegex """"<4> #Primitives.selector get FsUi/darkMode": "2","""
                    Cy2.waitForRegex """"<5> FsUi/darkMode get": "1","""
                    Cy2.waitForRegex """"<6> #Primitives.selector get FsUi/fontSize": "2","""
                    Cy2.waitForRegex """"<7> FsUi/fontSize get": "1","""
                    Cy2.waitForRegex """"<8> #Primitives.selector get FsUi/systemUiFont": "2","""
                    Cy2.waitForRegex """"<9> FsUi/systemUiFont get": "1","""
                    Cy2.waitForRegex """"<10> #Primitives.selector get FsBeacon/asyncMessageIdAtoms": "1","""
                    Cy2.waitForRegex """"<11> #Primitives.selector get FsStore/asyncFileIdAtoms": "1","""
                    Cy2.waitForRegex """"<12> #Primitives.selector get FsStore/privateKeys": "1","""
                    Cy2.waitForRegex """"<13> #Primitives.asyncSelector get FsStore/asyncAlias": "1"[^,]"""

                    Cy2.clickText "clear logs"
                    Cy2.clickText "sign in"

                    Cy2.waitForRegex """"<0> @FsUi/fontSize": "0","""
                    Cy2.waitForRegex """"<1> @FsUi/systemUiFont": "0","""
                    Cy2.waitForRegex """"<2> #Primitives.selector get FsStore/gunUser": "1","""
                    Cy2.waitForRegex """"<3> #Primitives.selector get FsStore/alias": "1","""
                    Cy2.waitForRegex """"<4> #Primitives.selector get FsUi/darkMode": "4","""
                    Cy2.waitForRegex """"<5> #Primitives.selector get FsStore/gunAtomNode": "11","""
                    Cy2.waitForRegex """"<6> #Primitives.selector get FsStore/gunNamespace": "1","""
                    Cy2.waitForRegex """"<7> #Primitives.selector get FsStore/hub": "2","""
                    Cy2.waitForRegex """"<8> #Primitives.selector get FsStore/hubConnection": "1","""
                    Cy2.waitForRegex """"<9> #Primitives.selector get FsStore/lastSyncValueByTypeAtom": "3","""
                    Cy2.waitForRegex """"<10> FsUi/darkMode get": "2","""
                    Cy2.waitForRegex """"<11> #Primitives.selector get FsUi/fontSize": "3","""
                    Cy2.waitForRegex """"<12> FsUi/fontSize get": "1","""
                    Cy2.waitForRegex """"<13> #Primitives.selector get FsUi/systemUiFont": "3","""
                    Cy2.waitForRegex """"<14> FsUi/systemUiFont get": "1","""
                    Cy2.waitForRegex """"<15> #Primitives.selector get FsBeacon/asyncMessageIdAtoms": "2","""
                    Cy2.waitForRegex """"<16> #Primitives.selector get FsStore/asyncFileIdAtoms": "2","""
                    Cy2.waitForRegex """"<17> #Primitives.selector get FsStore/privateKeys": "1","""
                    Cy2.waitForRegex """"<18> #Primitives.asyncSelector get FsStore/asyncAlias": "1","""
                    Cy2.waitForRegex """"<19> @FsStore/Message/00000000-0000-0000-0000-000000000000/ack": "1","""
                    Cy2.waitForRegex """"<20> @FsStore/File/00000000-0000-0000-0000-000000000000/pub": "1"[^,]"""

                    Cy2.clickText "clear logs"
                    Cy2.clickText "add file"

                    Cy2.waitForRegex
                        """"<0> #Primitives.registerAtom FsStore/File/.+/chunkCount atom129 AtomWithSync": "1","""

                    Cy2.waitForRegex """"<1> #Primitives.selector set FsStore/File/.+/chunkCount": "1","""
                    Cy2.waitForRegex """"<2> #Primitives.selector get FsStore/gunAtomNode": "11","""
                    Cy2.waitForRegex """"<3> FsStore/File/.+/chunkCount set": "1","""

                    Cy2.waitForRegex
                        """"<4> #Primitives.registerAtom FsStore/File/.+/0/chunk atom135 AtomWithSync": "1","""

                    Cy2.waitForRegex """"<5> #Primitives.selector set FsStore/File/.+/0/chunk": "1","""
                    Cy2.waitForRegex """"<6> FsStore/File/.+/0/chunk set": "1","""

                    Cy2.waitForRegex
                        """"<7> #Primitives.registerAtom FsStore/File/.+/1/chunk atom141 AtomWithSync": "1","""

                    Cy2.waitForRegex """"<8> #Primitives.selector set FsStore/File/.+/1/chunk": "1","""
                    Cy2.waitForRegex """"<9> FsStore/File/.+/1/chunk set": "1","""

                    Cy2.waitForRegex
                        """"<10> #Primitives.registerAtom FsStore/File/.+/2/chunk atom147 AtomWithSync": "1","""

                    Cy2.waitForRegex """"<11> #Primitives.selector set FsStore/File/.+/2/chunk": "1","""
                    Cy2.waitForRegex """"<12> FsStore/File/.+/2/chunk set": "1","""

                    Cy2.waitForRegex
                        """"<13> #Primitives.registerAtom FsStore/File/.+/3/chunk atom153 AtomWithSync": "1","""

                    Cy2.waitForRegex """"<14> #Primitives.selector set FsStore/File/.+/3/chunk": "1","""
                    Cy2.waitForRegex """"<15> FsStore/File/.+/3/chunk set": "1","""

                    Cy2.waitForRegex
                        """"<16> #Primitives.registerAtom FsStore/File/.+/pub atom159 AtomWithSync": "1","""

                    Cy2.waitForRegex """"<17> #Primitives.selector set FsStore/File/.+/pub": "1","""
                    Cy2.waitForRegex """"<18> FsStore/File/.+/pub set": "1"[^,]"""

                    Cy2.clickText "clear logs"
                    Cy2.clickText "unmount"

                    Cy2.waitForRegex """"<0> @FsStore/Message/00000000-0000-0000-0000-000000000000/ack": "-1","""
                    Cy2.waitForRegex """"<1> @FsStore/File/00000000-0000-0000-0000-000000000000/pub": "-1","""
                    Cy2.waitForRegex """"<2> @FsUi/fontSize": "0","""
                    Cy2.waitForRegex """"<3> @FsUi/systemUiFont": "0","""
                    Cy2.waitForRegex """"<4> #Primitives.selector get FsUi/fontSize": "1","""
                    Cy2.waitForRegex """"<5> FsUi/fontSize get": "1","""
                    Cy2.waitForRegex """"<6> #Primitives.selector get FsUi/systemUiFont": "1","""
                    Cy2.waitForRegex """"<7> FsUi/systemUiFont get": "1","""
                    Cy2.waitForRegex """"<8> >> atomPath=FsUi/fontSize alias=a.* subscribe": "1","""
                    Cy2.waitForRegex """"<9> >> atomPath=FsUi/systemUiFont alias=a.* subscribe": "1"[^,]"""

                    Cy2.clickText "clear logs"
                    Cy2.clickText "mount"

                    Cy2.waitForRegex """"<0> @FsUi/fontSize": "0","""
                    Cy2.waitForRegex """"<1> @FsUi/systemUiFont": "0","""
                    Cy2.waitForRegex """"<2> #Primitives.selector get FsStore/gunUser": "1","""
                    Cy2.waitForRegex """"<3> #Primitives.selector get FsStore/alias": "1","""
                    Cy2.waitForRegex """"<4> #Primitives.selector get FsUi/darkMode": "4","""
                    Cy2.waitForRegex """"<5> #Primitives.selector get FsStore/gunNamespace": "1","""
                    Cy2.waitForRegex """"<6> #Primitives.selector get FsStore/gunAtomNode": "10","""
                    Cy2.waitForRegex """"<7> #Primitives.selector get FsStore/hubConnection": "1","""
                    Cy2.waitForRegex """"<8> FsUi/darkMode get": "2","""
                    Cy2.waitForRegex """"<9> #Primitives.selector get FsStore/hub": "2","""
                    Cy2.waitForRegex """"<10> #Primitives.selector get FsUi/fontSize": "2","""
                    Cy2.waitForRegex """"<11> #Primitives.selector get FsUi/systemUiFont": "2","""
                    Cy2.waitForRegex """"<12> #Primitives.selector get FsBeacon/asyncMessageIdAtoms": "2","""
                    Cy2.waitForRegex """"<13> #Primitives.selector get FsStore/asyncFileIdAtoms": "2","""
                    Cy2.waitForRegex """"<14> #Primitives.selector get FsStore/privateKeys": "1","""
                    Cy2.waitForRegex """"<15> #Primitives.asyncSelector get FsStore/asyncAlias": "1"[^,]"""

                    Cy2.clickText "clear logs"
                    Cy2.clickText "unmount"

                    Cy2.waitForRegex """"<0> @FsUi/fontSize": "0","""
                    Cy2.waitForRegex """"<1> @FsUi/systemUiFont": "0","""
                    Cy2.waitForRegex """"<2> #Primitives.selector get FsUi/fontSize": "1","""
                    Cy2.waitForRegex """"<3> FsUi/fontSize get": "1","""
                    Cy2.waitForRegex """"<4> #Primitives.selector get FsUi/systemUiFont": "1","""
                    Cy2.waitForRegex """"<5> FsUi/systemUiFont get": "1"[^,]"""

                    Cy2.clickText "clear logs"
                    Cy2.clickText "mount"

                    Cy2.waitForRegex """"<0> @FsUi/fontSize": "0","""
                    Cy2.waitForRegex """"<1> @FsUi/systemUiFont": "0","""
                    Cy2.waitForRegex """"<2> #Primitives.selector get FsStore/gunUser": "1","""
                    Cy2.waitForRegex """"<3> #Primitives.selector get FsStore/alias": "1","""
                    Cy2.waitForRegex """"<4> #Primitives.selector get FsUi/darkMode": "4","""
                    Cy2.waitForRegex """"<5> #Primitives.selector get FsStore/gunNamespace": "1","""
                    Cy2.waitForRegex """"<6> #Primitives.selector get FsStore/gunAtomNode": "4","""
                    Cy2.waitForRegex """"<7> #Primitives.selector get FsStore/hubConnection": "1","""
                    Cy2.waitForRegex """"<8> FsUi/darkMode get": "2","""
                    Cy2.waitForRegex """"<9> #Primitives.selector get FsStore/hub": "2","""
                    Cy2.waitForRegex """"<10> #Primitives.selector get FsUi/fontSize": "2","""
                    Cy2.waitForRegex """"<11> #Primitives.selector get FsUi/systemUiFont": "2","""
                    Cy2.waitForRegex """"<12> #Primitives.selector get FsBeacon/asyncMessageIdAtoms": "2","""
                    Cy2.waitForRegex """"<13> #Primitives.selector get FsStore/asyncFileIdAtoms": "2","""
                    Cy2.waitForRegex """"<14> #Primitives.selector get FsStore/privateKeys": "1","""
                    Cy2.waitForRegex """"<15> #Primitives.asyncSelector get FsStore/asyncAlias": "1"[^,]"""

                    Cy2.clickText "clear logs"
                    Cy2.clickText "logout"

                    Cy2.waitForRegex """"<0> #Primitives.selector get FsStore/gunUser": "1","""
                    Cy2.waitForRegex """"<1> #Primitives.selector get FsStore/alias": "1","""
                    Cy2.waitForRegex """"<2> #Primitives.selector get FsUi/darkMode": "2","""
                    Cy2.waitForRegex """"<3> FsUi/darkMode get": "1","""
                    Cy2.waitForRegex """"<4> #Primitives.selector get FsUi/fontSize": "2","""
                    Cy2.waitForRegex """"<5> #Primitives.selector get FsStore/hub": "1","""
                    Cy2.waitForRegex """"<6> #Primitives.selector get FsStore/hubConnection": "1","""
                    Cy2.waitForRegex """"<7> FsUi/fontSize get": "1","""
                    Cy2.waitForRegex """"<8> #Primitives.selector get FsUi/systemUiFont": "2","""
                    Cy2.waitForRegex """"<9> FsUi/systemUiFont get": "1","""
                    Cy2.waitForRegex """"<10> #Primitives.selector get FsBeacon/asyncMessageIdAtoms": "1","""
                    Cy2.waitForRegex """"<11> #Primitives.selector get FsStore/asyncFileIdAtoms": "1","""
                    Cy2.waitForRegex """"<12> #Primitives.selector get FsStore/privateKeys": "1","""
                    Cy2.waitForRegex """"<13> #Primitives.asyncSelector get FsStore/asyncAlias": "1"[^,]"""

                    Cy2.clickText "clear logs"
                    Cy2.clickText "unmount"

                    Cy2.waitForRegex """"<0> @FsUi/fontSize": "0","""
                    Cy2.waitForRegex """"<1> @FsUi/systemUiFont": "0"[^,]"""

                    ))

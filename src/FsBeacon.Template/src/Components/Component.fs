namespace FsBeacon.Template.Components


open System
open System.Collections.Generic
open Browser.Types
open Fable.Core
open Fable.React
open Feliz
open FsCore.Model
open FsJs
open FsStore
open FsStore.State
open FsStore.Bindings
open FsStore.Hooks
open FsUi.Bindings
open FsUi.Hooks
open FsUi.State
open FsUi.Components


module State =
    module FsBeacon =
        let root = StoreRoot (nameof FsBeacon)

    //    let rec asyncFileIdAtoms =
//        Store.selectAtomSyncKeys
//            FsBeacon.root
//            (nameof asyncFileIdAtoms)
//            Atoms.File.chunkCount
//            (FileId Guid.Empty)
//            (Guid >> FileId)

    module Atoms =
        let rec hydrateStarted = Store.atom FsBeacon.root (nameof hydrateStarted) false
        let rec signInStarted = Store.atom FsBeacon.root (nameof signInStarted) false

        module Device =
            let rec fileId: (DeviceId -> Jotai.Atom<FileId>) =
                Store.atomFamilyWithSync
                    FsBeacon.root
                    Atoms.Device.collection
                    (nameof fileId)
                    (fun (_: DeviceId) -> FileId Guid.Empty)
                    Atoms.Device.deviceIdIdentifier

    module Selectors =
        let rec asyncDeviceIdAtoms =
            Store.selectAtomSyncKeys
                FsBeacon.root
                (nameof asyncDeviceIdAtoms)
                Atoms.Device.fileId
                Dom.deviceInfo.DeviceId
                (Guid >> DeviceId)

module Component =
    let dataChar = "#"
    let dataBlob = Fable.SimpleHttp.Blob.fromText (String.init (Hydrate.fileChunkSize * 2) (fun _ -> dataChar))
    let hexStringPromise = Js.blobToHexString dataBlob


    [<ReactComponent>]
    let File i fileId =
        let progress = Store.useValue (Selectors.File.progress fileId)

        Ui.stack
            (fun _ -> ())
            [
                Ui.box
                    (fun _ -> ())
                    [
                        str $"{Browser.Dom.window.location.port}:file[{i}]={progress}%%"
                    ]

                Button.Button
                    {|
                        Icon = Some (Icons.bi.BiSave |> Icons.render, Button.IconPosition.Left)
                        Hint = None
                        Props = fun _ -> ()
                        Children =
                            [
                                str $"{Browser.Dom.window.location.port}:file[{i}]:save"
                            ]
                    |}

                Button.Button
                    {|
                        Icon = Some (Icons.bi.BiTrash |> Icons.render, Button.IconPosition.Left)
                        Hint = None
                        Props = fun _ -> ()
                        Children =
                            [
                                str $"{Browser.Dom.window.location.port}:file[{i}]:delete"
                            ]
                    |}
            ]

    [<ReactComponent>]
    let Device (deviceIdAtom: Jotai.Atom<DeviceId>) =
        let deviceId = Store.useValue deviceIdAtom
        let fileId = Store.useValue (State.Atoms.Device.fileId deviceId)
        //        let progress = Store.useValue (Selectors.File.progress fileId)
        Ui.stack
            (fun _ -> ())
            [
                Ui.box
                    (fun _ -> ())
                    [
                        str $"{Browser.Dom.window.location.port}:file[{0}]=0%%"
                    ]

                File 0 fileId

            //                                                                    Button.Button
//                                                                        {|
//                                                                            Icon =
//                                                                                Some (Icons.bi.BiSave |> Icons.render, Button.IconPosition.Left)
//                                                                            Hint = None
//                                                                            Props = fun _ -> ()
//                                                                            Children =
//                                                                                [
//                                                                                    str $"{Browser.Dom.window.location.port}:file[{i}]:save"
//                                                                                ]
//                                                                        |}
//
//                                                                    Button.Button
//                                                                        {|
//                                                                            Icon =
//                                                                                Some (
//                                                                                    Icons.bi.BiTrash |> Icons.render,
//                                                                                    Button.IconPosition.Left
//                                                                                )
//                                                                            Hint = None
//                                                                            Props = fun _ -> ()
//                                                                            Children =
//                                                                                [
//                                                                                    str $"{Browser.Dom.window.location.port}:file[{i}]:delete"
//                                                                                ]
//                                                                        |}
            ]

    [<ReactComponent>]
    let HydrateCoreContainer () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "HydrateCoreContainer.render")

        Jotai.jotaiUtils.useHydrateAtoms [|
            unbox Atoms.showDebug, unbox true
            unbox Atoms.logLevel, unbox Dom.LogLevel.Debug
        |]

        nothing

    [<ReactComponent>]
    let HydrateSyncContainer () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "HydrateSyncContainer.render")

        Jotai.jotaiUtils.useHydrateAtoms [|
            unbox Atoms.gunOptions,
            unbox (
                Model.GunOptions.Sync [|
                    Gun.GunPeer "https://localhost:49221/gun"
                |]
            )
            //            unbox Atoms.hubUrl, unbox (Some "https://localhost:49211")
            |]

        nothing

    let deviceSignUpCache = Dictionary<DeviceId, bool> ()

    [<ReactComponent>]
    let SignInContainer () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "SignUpContainer.render")
        let deviceInfo = Store.useValue Selectors.deviceInfo

        let callbacks = Store.useCallbacks ()
        let signIn = Auth.useSignIn ()
        let signUp = Auth.useSignUp ()

        let toast = Ui.useToast ()

        React.useEffectOnce
            (fun () ->
                promise {
                    if not (deviceSignUpCache.ContainsKey deviceInfo.DeviceId) then
                        deviceSignUpCache.[deviceInfo.DeviceId] <- true

                        let! _getter, _setter = callbacks ()

                        let credentials = $"a@{Dom.deviceTag}"
                        //
//                            match! signIn (credentials, credentials) with
//                            | Ok _ -> ()
//                            | Error error ->
//                                toast (fun x -> x.description <- $"1: {error}")
//
//                                match! signUp (credentials, credentials) with
//                                | Ok _ -> ()
//                                | Error error -> toast (fun x -> x.description <- $"2: {error}")

                        match! signUp (credentials, credentials) with
                        | Ok _ -> ()
                        | Error error when error.Contains "User already created" ->
                            //                                do! Promise.sleep 300
                            match! signIn (credentials, credentials) with
                            | Ok _ -> ()
                            | Error error -> toast (fun x -> x.description <- $"1: {error}")
                        | Error error -> toast (fun x -> x.description <- $"2: {error}")

                        //                            let gun = Store.value getter Selectors.Gun.gun
//                            let user = gun.user()
//                            let! ack = Gun.createUser user (Gun.Alias deviceId) (Gun.Pass deviceId)
//                            printfn $"ack={ack}"


                        //                        let! hexString = hexStringPromise
//                        let fileId = Hydrate.hydrateFile setter (Model.AtomScope.Current, hexString)
//
//                        Store.set setter (State.Device.fileId deviceInfo.DeviceId) fileId

                        let fileId = null

                        logger.Info
                            (fun () -> $"Component.HydrateContainer().useEffectOnce() fileId={fileId} (currently null)")
                }
                |> Promise.start)

        nothing


    [<ReactComponent>]
    let AsyncAliasIndicator () =
        let asyncAlias = Store.useValue Selectors.Gun.asyncAlias

        Ui.flex
            (fun x -> x.flex <- "1")
            [
                str $"async alias: {asyncAlias}"
            ]

    [<ReactComponent>]
    let InnerComponent () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "Component.render")
        let deviceInfo = Store.useValue Selectors.deviceInfo
        let gunOptions = Store.useValue Atoms.gunOptions
        let gunPeers = Store.useValue Selectors.Gun.gunPeers
        let hubUrl = Store.useValue Atoms.hubUrl
        let uiState = Store.useValue Selectors.Ui.uiState
        let sessionRestored = Store.useValue Atoms.sessionRestored
        let showDebug = Store.useValue Atoms.showDebug
        let alias = Store.useValue Selectors.Gun.alias
        let privateKeys = Store.useValue Selectors.Gun.privateKeys
        //        let fileIdAtoms = Store.useValue State.asyncFileIdAtoms
        let deviceIdAtoms = Store.useValue State.Selectors.asyncDeviceIdAtoms

        let signIn = Auth.useSignIn ()

        let onKeysChange =
            Store.useCallbackRef
                (fun _ _ key ->
                    promise {
                        let! signIn = signIn ("", key)

                        logger.Warning (fun () -> $"test: signIn={JS.JSON.stringify signIn}")
                    })

        let setHydrateStarted = Store.useSetState State.Atoms.hydrateStarted
        let setSignInStarted = Store.useSetState State.Atoms.signInStarted

        let debouncedOnKeysChange =
            React.useMemo (
                (fun () -> Js.debounce onKeysChange 50),
                [|
                    box onKeysChange
                |]
            )

        Ui.box
            (fun x ->
                x.id <- "component"
                x.fontSize <- "11px"
                x.margin <- "15px")
            [
                Button.Button
                    {|
                        Hint = None
                        Icon = Some (Icons.bi.BiData |> Icons.render, Button.IconPosition.Left)
                        Props =
                            fun x ->
                                x.borderRadius <- "0 5px 5px 0"
                                x.minWidth <- "26px"

                                x.onClick <- (fun _ -> promise { setHydrateStarted true })
                        Children =
                            [
                                str "hydrate"
                            ]
                    |}

                Button.Button
                    {|
                        Hint = None
                        Icon = Some (Icons.io5.IoKey |> Icons.render, Button.IconPosition.Left)
                        Props =
                            fun x ->
                                x.borderRadius <- "0 5px 5px 0"
                                x.minWidth <- "26px"

                                x.onClick <- (fun _ -> promise { setSignInStarted true })
                        Children =
                            [
                                str "sign in"
                            ]
                    |}

                Ui.box
                    (fun _ -> ())
                    [
                        str $"#2 {Browser.Dom.window.location.href}"
                        br []
                        str $">>{deviceIdAtoms}<<"
                    ]

                React.suspense (
                    [
                        AsyncAliasIndicator ()
                    ],
                    LoadingSpinner.InlineLoadingSpinner ()
                )

                Ui.stack
                    (fun x -> x.spacing <- "15px")
                    [
                        Ui.box
                            (fun _ -> ())
                            [
                                str
                                    $" {Json.encodeWithNullFormatted
                                            {|
                                                DeviceInfo = deviceInfo
                                                GunOptions = gunOptions
                                                GunPeers = gunPeers
                                                HubUrl = hubUrl
                                                UiState = uiState
                                                SessionRestored = sessionRestored
                                                ShowDebug = showDebug
                                                Alias = alias
                                                PrivateKeys = privateKeys
                                            |}}"
                            ]

                        Input.Input
                            {|
                                CustomProps = fun _ -> ()
                                Props =
                                    fun x ->
                                        x.placeholder <- "Private Keys"
                                        x.id <- "privateKeys"

                                        x.onChange <- fun (x: KeyboardEvent) -> debouncedOnKeysChange x.Value
                            |}

                        yield! deviceIdAtoms |> Array.map Device
                    ]

                DebugPanel.DebugPanel DebugPanel.DebugPanelDisplay.Inline
            ]


    [<ReactComponent>]
    let Component () =
        let hydrateStarted = Store.useValue State.Atoms.hydrateStarted
        let signInStarted = Store.useValue State.Atoms.signInStarted

        React.fragment [
            HydrateCoreContainer ()
            if hydrateStarted then HydrateSyncContainer ()
            if signInStarted then SignInContainer ()
            InnerComponent ()
        ]

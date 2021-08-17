namespace FsBeacon.Template.Components


open System.Collections.Generic
open Fable.Core
open Fable.React
open Feliz
open FsCore.Model
open FsJs
open FsStore
open FsStore.State
open FsStore.Bindings
open FsStore.Hooks
open FsBeacon.Template.State
open FsUi.Bindings
open FsUi.Hooks
open FsUi.State
open FsUi.Components


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
        let fileId = Store.useValue (Atoms.Device.fileId deviceId)
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
            (fun _ -> ())
            [
                str $"async alias: {asyncAlias}"
            ]

    [<ReactComponent>]
    let InnerComponent () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "Component.render")
        let toast = Ui.useToast ()
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
        let deviceIdAtoms = Store.useValue Selectors.asyncDeviceIdAtoms

        let signIn = Auth.useSignIn ()

        let onKeysChange =
            Store.useCallbackRef
                (fun _ _ key ->
                    promise {
                        let! signIn = signIn ("", key)

                        logger.Warning (fun () -> $"test: signIn={JS.JSON.stringify signIn}")

                        match signIn with
                        | Ok (_alias, _keys) -> ()
                        | Error error -> toast (fun x -> x.description <- error)
                    })

        let setHydrateStarted = Store.useSetState Atoms.hydrateStarted
        let setSignInStarted = Store.useSetState Atoms.signInStarted

        Ui.stack
            (fun x ->
                x.id <- "component"
                x.fontSize <- "11px"
                x.maxWidth <- "100vw"
                x.padding <- "15px"
                x.flex <- "1")
            [
                Button.Button
                    {|
                        Hint = None
                        Icon = Some (Icons.bi.BiData |> Icons.render, Button.IconPosition.Left)
                        Props = fun x -> x.onClick <- (fun _ -> promise { setHydrateStarted true })
                        Children =
                            [
                                str "hydrate"
                            ]
                    |}

                Button.Button
                    {|
                        Hint = None
                        Icon = Some (Icons.io5.IoKey |> Icons.render, Button.IconPosition.Left)
                        Props = fun x -> x.onClick <- (fun _ -> promise { setSignInStarted true })
                        Children =
                            [
                                str "sign in"
                            ]
                    |}

                Ui.flex
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
                        Ui.flex
                            (fun x -> x.flex <- "1")
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

                        yield! deviceIdAtoms |> Array.map Device
                    ]

                DebugPanel.DebugPanel DebugPanel.DebugPanelDisplay.Inline
            ]


    [<ReactComponent>]
    let Component () =
        let hydrateStarted = Store.useValue Atoms.hydrateStarted
        let signInStarted = Store.useValue Atoms.signInStarted

        React.fragment [
            HydrateCoreContainer ()
            if hydrateStarted then HydrateSyncContainer ()
            if signInStarted then SignInContainer ()
            InnerComponent ()
        ]

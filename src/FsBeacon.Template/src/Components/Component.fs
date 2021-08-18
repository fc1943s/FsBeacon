namespace FsBeacon.Template.Components

open FsCore
open Browser.Types
open Fable.React
open Feliz
open FsJs
open FsStore
open FsStore.Bindings.Gun
open FsStore.Model
open FsStore.State
open FsStore.Bindings
open FsStore.Hooks
open FsBeacon.Template.State
open FsBeacon.Template
open FsUi.Bindings
open FsUi.Hooks
open FsUi.State
open FsUi.Components


module Component =
    let dataChar = "#"
    let dataBlob = Fable.SimpleHttp.Blob.fromText (String.init (Hydrate.fileChunkSize * 2) (fun _ -> dataChar))
    let hexStringPromise = Js.blobToHexString dataBlob


    [<ReactComponent>]
    let File fileIdAtom =
        let fileId = Store.useValue fileIdAtom
        let privateKeys = Store.useValue Selectors.Gun.privateKeys
        let pub = Store.useValue (Atoms.File.pub fileId)
        let progress = Store.useValue (Selectors.File.progress fileId)

        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> $"File.render. fileId={fileId} progress={progress}")


        //        let valid, setValid = React.useState false
//
//        let valid =
//            React.useEffect (
//                (fun () ->
//                    promise {
//                        if progress >= 100 then
//                            let! hexString = hexStringPromise
//                            setValid (hexString = )
//                        ()
//                    }
//                    |> Promise.start),
//                [||]
//            )

        Ui.stack
            (fun _ -> ())
            [
                match privateKeys with
                | Some { pub = pub' } when pub' = pub -> str $"fileId={fileId} progress={progress}%%"
                | _ -> str $"fileId={fileId} progress=invalid"

            //                Button.Button
//                    {|
//                        Icon = Some (Icons.bi.BiSave |> Icons.render, Button.IconPosition.Left)
//                        Hint = None
//                        Props = fun _ -> ()
//                        Children =
//                            [
//                                str $"{Browser.Dom.window.location.port}:file[{fileId}]:save"
//                            ]
//                    |}
//
//                Button.Button
//                    {|
//                        Icon = Some (Icons.bi.BiTrash |> Icons.render, Button.IconPosition.Left)
//                        Hint = None
//                        Props = fun _ -> ()
//                        Children =
//                            [
//                                str $"{Browser.Dom.window.location.port}:file[{fileId}]:delete"
//                            ]
//                    |}
            ]


    [<ReactComponent>]
    let Files () =
        let logger = Store.useValue Selectors.logger
        let fileIdAtoms = Store.useValue Selectors.asyncFileIdAtoms
        logger.Info (fun () -> $"Files.render. fileIdAtoms.Length={fileIdAtoms.Length}")
        //        let fileIdAtoms = Store.useValue State.Selectors.asyncFileIdAtoms

        React.fragment [
            yield! fileIdAtoms |> Array.map File
        ]


    [<ReactComponent>]
    let HydrateCoreContainer () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "HydrateCoreContainer.render")

        Jotai.jotaiUtils.useHydrateAtoms [|
            unbox Atoms.showDebug, unbox true
            unbox Atoms.logLevel, unbox Logger.LogLevel.Trace
        |]

        nothing

    [<ReactComponent>]
    let HydrateSyncContainer () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "HydrateSyncContainer.render")

        Jotai.jotaiUtils.useHydrateAtoms [|
            unbox Atoms.gunOptions,
            unbox (
                GunOptions.Sync [|
                    GunPeer "https://localhost:49221/gun"
                |]
            )
            //            unbox Atoms.hubUrl, unbox (Some "https://localhost:49211")
            |]

        let callbacks = Store.useCallbacks ()

        React.useEffect (
            (fun () ->
                promise {
                    let! _getter, setter = callbacks ()
                    Store.set setter Atoms.syncHydrateCompleted true
                }
                |> Promise.start),
            [|
                box callbacks
            |]
        )

        nothing


    [<ReactComponent>]
    let rec SignInContainer () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "SignInContainer.render")

        let deviceInfo = Store.useValue Selectors.deviceInfo

        let signIn = Auth.useSignIn ()
        let signUp = Auth.useSignUp ()

        let toast = Ui.useToast ()

        Store.useHashedEffectOnce
            (nameof SignInContainer, deviceInfo.DeviceId)
            (fun _getter _setter ->
                promise {
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
                })

        nothing


    [<ReactComponent>]
    let HydrateButton () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "HydrateButton.render")

        let syncHydrateCompleted = Store.useValue Atoms.syncHydrateCompleted
        let setSyncHydrateStarted = Store.useSetState Atoms.syncHydrateStarted

        Button.Button
            {|
                Hint = None
                Icon = Some (Icons.bi.BiData |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <- (fun _ -> promise { setSyncHydrateStarted true })
                        x.disabled <- syncHydrateCompleted
                Children =
                    [
                        str "hydrate"
                    ]
            |}

    [<ReactComponent>]
    let SignInButton () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "SignInButton.render")

        let alias = Store.useValue Selectors.Gun.alias
        let syncHydrateCompleted = Store.useValue Atoms.syncHydrateCompleted
        let setSignInStarted = Store.useSetState Atoms.signInStarted

        Button.Button
            {|
                Hint = None
                Icon = Some (Icons.io5.IoKey |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <- (fun _ -> promise { setSignInStarted true })
                        x.disabled <- not syncHydrateCompleted || alias.IsSome
                Children =
                    [
                        str "sign in"
                    ]
            |}

    [<ReactComponent>]
    let AddFileButton () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "AddFileButton.render")

        let alias = Store.useValue Selectors.Gun.alias
        let privateKeys = Store.useValue Selectors.Gun.privateKeys

        let addFile =
            Store.useCallbackRef
                (fun _ setter _ ->
                    promise {
                        printfn "add file!"

                        let! hexString = hexStringPromise
                        let fileId = Hydrate.hydrateFile setter (AtomScope.Current, hexString)

                        match fileId, privateKeys with
                        | Some fileId, Some privateKeys ->
                            Store.set setter (State.Atoms.File.pub fileId) privateKeys.pub
                        | _ -> logger.Error (fun () -> $"add file error. fileId={fileId}")
                    })

        Button.Button
            {|
                Hint = None
                Icon = Some (Icons.io5.IoAdd |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <- (fun _ -> addFile ())
                        x.disabled <- alias.IsNone
                Children =
                    [
                        str "add file"
                    ]
            |}

    [<ReactComponent>]
    let HrefIndicator () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "HrefIndicator.render")

        let _routeTrigger = Store.useValue Atoms.routeTrigger
        str $"href: {Browser.Dom.window.location.href}"

    [<ReactComponent>]
    let AsyncAliasIndicator () =
        let logger = Store.useValue Selectors.logger
        logger.Trace (fun () -> "AsyncAliasIndicator.render")

        let asyncAlias = Store.useValue Selectors.Gun.asyncAlias

        Ui.flex
            (fun _ -> ())
            [
                str $"async alias: {asyncAlias}"
            ]

    [<ReactComponent>]
    let SettingsIndicator () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "SettingsIndicator.render")

        let deviceInfo = Store.useValue Selectors.deviceInfo
        let gunOptions = Store.useValue Atoms.gunOptions
        let gunPeers = Store.useValue Selectors.Gun.gunPeers
        let hubUrl = Store.useValue Atoms.hubUrl
        let uiState = Store.useValue Selectors.Ui.uiState
        let sessionRestored = Store.useValue Atoms.sessionRestored
        let showDebug = Store.useValue Atoms.showDebug
        let alias = Store.useValue Selectors.Gun.alias
        let privateKeys = Store.useValue Selectors.Gun.privateKeys

        let text =
            React.useMemo (
                (fun () ->
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
                            |}}"),
                [|
                    box deviceInfo
                    box gunOptions
                    box gunPeers
                    box hubUrl
                    box uiState
                    box sessionRestored
                    box showDebug
                    box alias
                    box privateKeys
                |]
            )

        Ui.flex
            (fun x ->
                x.flex <- "1"
                x.whiteSpace <- "pre-wrap")
            [
                str text
            ]

    [<ReactComponent>]
    let InnerComponent () =
        let logger = Store.useValue Selectors.logger
        logger.Trace (fun () -> "InnerComponent.render")

        React.useEffect (
            (fun () ->
                match Dom.window () with
                | Some window ->
                    window.scrollTo (
                        {|
                            left = 0.
                            top = 0.
                            behavior = ScrollBehavior.Smooth
                        |}: ScrollToOptions
                    )
                | None -> ()),
            [||]
        )

        Ui.stack
            (fun x ->
                x.id <- "component"
                x.alignItems <- "flex-start"
                x.fontSize <- "11px"
                x.maxWidth <- "100vw"
                x.padding <- "15px"
                x.flex <- "1")
            [
                HydrateButton ()

                SignInButton ()

                AddFileButton ()

                HrefIndicator ()

                React.suspense (
                    [
                        AsyncAliasIndicator ()
                    ],
                    LoadingSpinner.InlineLoadingSpinner ()
                )

                Files ()

                SettingsIndicator ()

                DebugPanel.DebugPanel DebugPanel.DebugPanelDisplay.Inline
            ]


    [<ReactComponent>]
    let MessageProcessor messageIdAtom =
        let logger = Store.useValue Selectors.logger
        logger.Trace (fun () -> "MessageProcessor.render")
        let messageProcessor = Messaging.useMessageProcessor ()
        let messageId = Store.useValue messageIdAtom
        let message = Store.useValue (Atoms.Message.message messageId)
        let ack, setAck = Store.useState (Atoms.Message.ack messageId)

        React.useEffect (
            (fun () ->
                messageProcessor (ack, (fun () -> setAck (Some true)), message)
                |> Promise.start),
            [|
                box messageProcessor
                box message
                box ack
                box setAck
            |]
        )

        nothing

    [<ReactComponent>]
    let MessagesListener () =
        let logger = Store.useValue Selectors.logger
        logger.Trace (fun () -> "MessagesListener.render")
        let messageIdAtoms = Store.useValue State.Selectors.asyncMessageIdAtoms

        React.fragment [
            yield! messageIdAtoms |> Array.map MessageProcessor
        ]

    [<ReactComponent>]
    let HydrateContainer () =
        let syncHydrateStarted = Store.useValue Atoms.syncHydrateStarted

        React.fragment [
            HydrateCoreContainer ()
            if syncHydrateStarted then HydrateSyncContainer ()
        ]

    [<ReactComponent>]
    let Component () =
        let logger = Store.useValue Selectors.logger
        logger.Trace (fun () -> "Component.render")
        let signInStarted = Store.useValue Atoms.signInStarted

        React.fragment [
            HydrateContainer ()
            MessagesListener ()
            if signInStarted then SignInContainer ()
            InnerComponent ()
        ]

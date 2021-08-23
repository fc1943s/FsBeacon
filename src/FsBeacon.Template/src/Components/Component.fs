namespace FsBeacon.Template.Components

open FsStore
open FsStore.Store
open FsCore
open Browser.Types
open Fable.React
open Feliz
open FsJs
open FsStore.Hooks
open FsStore.Bindings.Gun
open FsStore.Model
open FsStore.State
open FsStore.Bindings
open FsBeacon.Template.State
open FsBeacon.Template
open FsUi.Bindings
open FsUi.Hooks
open FsUi.State
open FsUi.Components


module Component =
    let dataChar = "#"
    let dataBlob = Fable.SimpleHttp.Blob.fromText (String.init (Hydrate.fileChunkSize * 1) (fun _ -> dataChar))
    let hexStringPromise = Js.blobToHexString dataBlob


    [<ReactComponent>]
    let File fileIdAtom =
        let fileId = Store.useValue fileIdAtom
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
                str $"fileId={fileId} progress={progress}%%"

            //                Button.Button
//                    {|
//                        Icon = Some (Icons.bi.BiSave |> Icons.render, Button.IconPosition.Left)
//                        Tooltip = None
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
//                        Tooltip = None
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
    let rec HydrateCoreContainer () =
        printfn "HydrateCoreContainer.render"

        Jotai.jotaiUtils.useHydrateAtoms [|
            unbox Atoms.showDebug, unbox true
            unbox Atoms.logLevel, unbox Logger.LogLevel.Trace
        |]

        //        Store.useHashedEffectOnce
//            (nameof HydrateCoreContainer)
//            (fun _ setter -> promise {
//            Atom.set setter Atoms.showDebug true
//            Atom.set setter Atoms.logLevel Logger.LogLevel.Trace
//        })

        nothing

    [<ReactComponent>]
    let rec HydrateSyncContainer () =
        printfn "HydrateSyncContainer.render"

        //        Jotai.jotaiUtils.useHydrateAtoms [|
//            unbox Atoms.gunOptions,
//            unbox (
//                GunOptions.Sync [|
//                    GunPeer "https://localhost:49221/gun"
//                |]
//            )
//            //            unbox Atoms.hubUrl, unbox (Some "https://localhost:49211")
//            |]

        Store.useHashedEffectOnce
            (nameof HydrateSyncContainer)
            (fun _ setter ->
                promise {
                    Atom.set
                        setter
                        Atoms.gunOptions
                        (GunOptions.Sync [|
                            GunPeer "https://localhost:49221/gun"
                         |])

                    Atom.set setter Atoms.syncHydrateCompleted true
                })

        nothing


    [<ReactComponent>]
    let rec SignInContainer () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "SignInContainer.render")

        let signUp = Auth.useSignUp ()

        let toast = Ui.useToast ()

        Store.useHashedEffectOnce
            (nameof SignInContainer)
            (fun getter setter ->
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
                        match! Auth.signIn getter setter (credentials, credentials) with
                        | Ok _ -> ()
                        | Error error -> toast (fun x -> x.description <- $"1: {error}")
                    | Error error -> toast (fun x -> x.description <- $"2: {error}")

                    //                            let gun = Atom.value getter Selectors.Gun.gun
//                            let user = gun.user()
//                            let! ack = Gun.createUser user (Gun.Alias deviceId) (Gun.Pass deviceId)
//                            printfn $"ack={ack}"


                    //                        let! hexString = hexStringPromise
//                        let fileId = Hydrate.hydrateFile setter (Model.AtomScope.Current, hexString)
//
//                        Atom.set setter (State.Device.fileId deviceInfo.DeviceId) fileId

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
                Tooltip = None
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
                Tooltip = None
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
    let LogoutButton () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "LogoutButton.render")

        let alias = Store.useValue Selectors.Gun.alias
        let logout = Auth.useLogout ()

        Button.Button
            {|
                Tooltip = None
                Icon = Some (Icons.io5.IoKey |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <- (fun _ -> logout ())
                        x.disabled <- alias.IsNone
                Children =
                    [
                        str "logout"
                    ]
            |}

    [<ReactComponent>]
    let ClearButton () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "ClearButton.render")

        Button.Button
            {|
                Tooltip = None
                Icon = Some (Icons.md.MdClear |> Icons.render, Button.IconPosition.Left)
                Props = fun x -> x.onClick <- (fun _ -> promise { Profiling.clearProfilingState () })
                Children =
                    [
                        str "clear logs"
                    ]
            |}

    //    let addFile
////        : (unit -> Fable.Core.JS.Promise<unit>)
//        = Store.rawSetSelector (fun getter setter newValue ->
//            ()
//    )

    [<ReactComponent>]
    let AddFileButton () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "AddFileButton.render")

        let alias = Store.useValue Selectors.Gun.alias

        let addFile =
            Store.useCallbackRef
                (fun _ setter _ ->
                    promise {
                        printfn "add file!"

                        let! hexString = hexStringPromise
                        let fileId = Hydrate.hydrateFile setter (AtomScope.Current, hexString)

                        Profiling.addCount $"addFile fileId={fileId}"
                    })

        Button.Button
            {|
                Tooltip = None
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
    let MountButton () =
        let logger = Store.useValue Selectors.logger
        logger.Info (fun () -> "MountButton.render")

        let mounted, setMounted = Store.useState State.Atoms.mounted

        Button.Button
            {|
                Tooltip = Some (str "Tooltip test")
                Icon = Some (Icons.io5.IoRefreshCircle |> Icons.render, Button.IconPosition.Left)
                Props = fun x -> x.onClick <- (fun _ -> promise { setMounted (not mounted) })
                Children =
                    [
                        str (if mounted then "unmount" else "mount")
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

        Ui.box
            (fun x ->
                x.flex <- "1"
                x.whiteSpace <- "pre-wrap")
            []

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
                x.flex <- "1")
            [
                Ui.stack
                    (fun _ -> ())
                    [
                        HydrateButton ()

                        SignInButton ()

                        AddFileButton ()

                        LogoutButton ()
                    ]

                HrefIndicator ()

                React.suspense (
                    [
                        AsyncAliasIndicator ()
                    ],
                    LoadingSpinner.InlineLoadingSpinner ()
                )

                Files ()

            //                SettingsIndicator ()
            ]


    [<ReactComponent>]
    let CommandConsumer messageIdAtom =
        let logger = Store.useValue Selectors.logger
        logger.Trace (fun () -> "CommandConsumer.render")
        let deviceInfo = Store.useValue Selectors.deviceInfo
        let appState = Store.useValue (Engine.appState deviceInfo.DeviceId)
        let consumeCommands = Store.useCallbackRef (Engine.consumeCommands Messaging.appUpdate appState)
        let messageId = Store.useValue messageIdAtom
        let appMessage = Store.useValue (Atoms.Message.appMessage messageId)
        let ack, setAck = Store.useState (Atoms.Message.ack messageId)

        React.useEffect (
            (fun () ->
                promise {
                    match ack with
                    | Some false ->
                        match appMessage with
                        | Message.Command command ->
                            let! events = consumeCommands (command |> List.singleton)

                            logger.Info
                                (fun () ->
                                    $"CommandConsumer. command processed. acked. command={command} events={events} ")

                            setAck (Some true)
                        | _ -> failwith "CommandConsumer. invalid command"

                    | _ -> ()
                }
                |> Promise.start),
            [|
                box logger
                box consumeCommands
                box appMessage
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
            yield! messageIdAtoms |> Array.map CommandConsumer
        ]

    [<ReactComponent>]
    let HydrateContainer () =
        let syncHydrateStarted = Store.useValue Atoms.syncHydrateStarted
        let syncHydrateCompleted = Store.useValue Atoms.syncHydrateCompleted

        React.fragment [
            if not syncHydrateCompleted then
                if syncHydrateStarted then HydrateSyncContainer ()
        ]

    [<ReactComponent>]
    let Component () =
        let logger = Store.useValue Selectors.logger
        logger.Trace (fun () -> "Component.render")
        let signInStarted = Store.useValue Atoms.signInStarted

        let mounted = Store.useValue State.Atoms.mounted

        Ui.stack
            (fun x -> x.padding <- "15px")
            [
                HydrateCoreContainer ()

                MountButton ()

                if mounted then
                    HydrateContainer ()
                    MessagesListener ()
                    if signInStarted then SignInContainer ()

                    InnerComponent ()

                ClearButton ()

                DebugPanel.DebugPanel DebugPanel.DebugPanelDisplay.Inline
            ]

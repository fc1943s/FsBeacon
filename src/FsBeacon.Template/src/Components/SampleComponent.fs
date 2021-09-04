namespace FsBeacon.Template.Components

open FsStore
open FsCore
open Browser.Types
open Fable.React
open Feliz
open FsJs
open FsStore.Hooks
open FsStore.Bindings.Gun
open FsStore.Model
open FsStore.State
open FsBeacon.Template.State
open FsBeacon.Template
open FsUi.Bindings
open FsUi.Hooks
open FsUi.State
open FsUi.Components


module SampleComponent =
    let dataChar = "#"
    let dataBlob = Fable.SimpleHttp.Blob.fromText (String.init (Hydrate.fileChunkSize * 1) (fun _ -> dataChar))
    let hexStringPromise = Js.blobToHexString dataBlob


    [<ReactComponent>]
    let File index fileIdAtom =
        let fileId = Store.useValue fileIdAtom
        let progress = Store.useValue (Selectors.File.progress fileId)

        Profiling.addTimestamp
            (fun () -> $"{nameof FsBeacon} | File [ render ] fileId={fileId} index={index + 1} progress={progress}")

        let deleteFile =
            Store.useCallbackRef
                (fun getter _setter _ ->
                    promise {
                        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | File [ deleteFile ] ")

                        let storeAtomPath =
                            RecordAtomPath (FsStore.storeRoot, Atoms.File.collection, Atoms.File.formatFileId fileId)

                        do! Engine.delete getter storeAtomPath
                    })

        Ui.flex
            (fun x -> x.whiteSpace <- "nowrap")
            [
                str $"fileId={fileId} index={index + 1} progress={progress}%%"

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
                Button.Button
                    {|
                        Icon = Some (Icons.bi.BiTrash |> Icons.render, Button.IconPosition.Left)
                        Tooltip = None
                        Props = fun x -> x.onClick <- (fun _ -> deleteFile ())
                        Children =
                            [
                                str $"[{index + 1}]:delete"
                            ]
                    |}
            ]


    [<ReactComponent>]
    let Files () =
        let fileIdAtoms = Store.useValue Selectors.Sample.fileIdAtoms

        Profiling.addTimestamp
            (fun () -> $"{nameof FsBeacon} | Files [ render ] fileIdAtoms.Length={fileIdAtoms.Length}")
        //        let fileIdAtoms = Store.useValue State.Selectors.asyncFileIdAtoms

        React.fragment [
            br []
            str $"file count: {fileIdAtoms.Length}"
            yield! fileIdAtoms |> Array.mapi File
        ]


    [<ReactComponent>]
    let rec HydrateCoreContainer () =
        Profiling.addTimestamp
            (fun () -> $"{nameof FsBeacon} | HydrateCoreContainer [ render ] hydrate trace from now on ")

        //        if Atoms.showDebug?init
//           |> Option.ofObjUnbox
//           |> Option.isNone then
//            failwith "invalid Atoms.showDebug init"
//
//        if Atoms.logLevel?init
//           |> Option.ofObjUnbox
//           |> Option.isNone then
//            failwith "invalid Atoms.logLevel init"

        //        Jotai.jotaiUtils.useHydrateAtoms [|
//            unbox Atoms.showDebug, unbox true
//            unbox Atoms.logLevel, unbox Logger.LogLevel.Trace
//        |]

        Store.useHashedEffectOnce
            (nameof HydrateCoreContainer)
            (fun _ setter ->
                promise {
                    Atom.set setter Atoms.showDebug true
                    Atom.set setter Atoms.logLevel Logger.LogLevel.Trace
                    Dom.globalDebug.Set true
                })

        nothing

    [<ReactComponent>]
    let rec HydrateSyncContainer () =
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | HydrateSyncContainer [ render ] ")

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

                    //                    Atom.set
//                        setter
//                        Atoms.hubUrl
//                        (Some "https://localhost:49211")

                    Profiling.addTimestamp
                        (fun () ->
                            $"{nameof FsBeacon} | HydrateSyncContainer [ render / useHashedEffectOnce ] gunOptions set manually ")

                    Atom.set setter Atoms.Sample.syncHydrateCompleted true
                })

        nothing


    [<ReactComponent>]
    let HydrateButton () =
        let syncHydrateCompleted = Store.useValue Atoms.Sample.syncHydrateCompleted
        let setSyncHydrateStarted = Store.useSetState Atoms.Sample.syncHydrateStarted

        Profiling.addTimestamp
            (fun () -> $"{nameof FsBeacon} | HydrateButton [ render ] syncHydrateCompleted={syncHydrateCompleted}")

        Button.Button
            {|
                Tooltip = None
                Icon = Some (Icons.bi.BiData |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <-
                            (fun _ ->
                                promise {
                                    Profiling.addTimestamp
                                        (fun () ->
                                            $"{nameof FsBeacon} | HydrateButton [ onClick ] syncHydrateCompleted={syncHydrateCompleted}")

                                    setSyncHydrateStarted true
                                })

                        x.disabled <- syncHydrateCompleted
                Children =
                    [
                        str "hydrate"
                    ]
            |}

    [<ReactComponent>]
    let SignInButton () =
        let alias = Store.useValue Selectors.Gun.alias
        let syncHydrateCompleted = Store.useValue Atoms.Sample.syncHydrateCompleted

        let logger = Store.useValue Selectors.logger

        let signUp = Auth.useSignUp ()
        let _ = Auth.useGunAliasLoader ()

        let toast = Ui.useToast ()

        Profiling.addTimestamp
            (fun () ->
                $"{nameof FsBeacon} | SignInButton [ render ] alias={alias} syncHydrateCompleted={syncHydrateCompleted}")

        let signIn =
            Store.useCallbackRef
                (fun getter setter _ ->
                    promise {
                        Profiling.addTimestamp
                            (fun () -> $"{nameof FsBeacon} | SignInContainer [ render ] starting sign up...")

                        let credentials = $"alias@{Dom.deviceTag}"
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

        Button.Button
            {|
                Tooltip = None
                Icon = Some (Icons.io5.IoKey |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <-
                            (fun _ ->
                                promise {
                                    Profiling.addTimestamp
                                        (fun () ->
                                            $"{nameof FsBeacon} | SignInButton [ onClick ] syncHydrateCompleted={syncHydrateCompleted}")

                                    do! signIn ()
                                })

                        x.disabled <- not syncHydrateCompleted || alias.IsSome
                Children =
                    [
                        str "sign in"
                    ]
            |}

    [<ReactComponent>]
    let LogoutButton () =
        let alias = Store.useValue Selectors.Gun.alias
        let logout = Auth.useLogout ()

        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | LogoutButton [ render ] alias={alias}")

        Button.Button
            {|
                Tooltip = None
                Icon = Some (Icons.io5.IoKey |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <-
                            (fun _ ->
                                Profiling.addTimestamp
                                    (fun () -> $"{nameof FsBeacon} | LogoutButton [ onClick ] alias={alias}")

                                logout ())

                        x.disabled <- alias.IsNone
                Children =
                    [
                        str $"logout ({alias})"
                    ]
            |}

    [<ReactComponent>]
    let ClearLogsButton () =
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | ClearLogsButton [ render ]")

        Button.Button
            {|
                Tooltip = None
                Icon = Some (Icons.bi.BiRecycle |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <-
                            (fun _ ->
                                promise {
                                    Profiling.addTimestamp
                                        (fun () -> $"{nameof FsBeacon} | ClearLogsButton [ onClick ]")

                                    Profiling.globalClearProfilingState.Get () ()
                                })
                Children =
                    [
                        str "clear logs"
                    ]
            |}

    [<ReactComponent>]
    let ToggleLogsButton () =
        let showDebug, setShowDebug = Store.useState Atoms.showDebug
        let logLevel, setLogLevel = Store.useState Atoms.logLevel

        let getLocals () =
            $"showDebug={showDebug} logLevel={logLevel}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp
                (fun () -> $"{nameof FsBeacon} | SampleComponent.ToggleLogsButton {fn ()} {getLocals ()}")

        Button.Button
            {|
                Tooltip = None
                Icon = Some (Icons.bi.BiRecycle |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <-
                            (fun _ ->
                                promise {
                                    addTimestamp (fun () -> "[ onClick ]") getLocals

                                    setShowDebug (not showDebug)

                                    setLogLevel (
                                        if logLevel = Logger.LogLevel.Trace then
                                            Logger.LogLevel.Debug
                                        else
                                            Logger.LogLevel.Trace
                                    )

                                    Dom.globalDebug.Set (not showDebug)
                                })
                Children =
                    [
                        str $"""{if not showDebug then "enable" else "disable"} logs"""
                    ]
            |}

    [<ReactComponent>]
    let AddFileButton () =
        let alias = Store.useValue Selectors.Gun.alias

        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | AddFileButton [ render ] alias={alias}")

        let addFile =
            Store.useCallbackRef
                (fun _ setter _ ->
                    promise {
                        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | AddFileButton [ render ] addFile()")

                        let! hexString = hexStringPromise
                        let fileId = Hydrate.hydrateFile setter hexString

                        Profiling.addTimestamp
                            (fun () -> $"{nameof FsBeacon} | addFile callback completed. fileId={fileId}")
                    })

        Button.Button
            {|
                Tooltip = None
                Icon = Some (Icons.io5.IoAdd |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <-
                            (fun _ ->
                                Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | AddFileButton [ onClick ]")
                                addFile ())

                        x.disabled <- alias.IsNone
                Children =
                    [
                        str "add file"
                    ]
            |}

    [<ReactComponent>]
    let CounterButton () =
        let testCounter, setTestCounter = Store.useState State.Atoms.Sample.testCounter

        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | CounterButton [ render ] testCounter={testCounter}")

        Button.Button
            {|
                Tooltip = Some (str "Tooltip test")
                Icon = Some (Icons.io5.IoAdd |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <-
                            (fun _ ->
                                promise {
                                    Profiling.addTimestamp
                                        (fun () ->
                                            $"{nameof FsBeacon} | CounterButton [ onClick ] testCounter={testCounter}")

                                    setTestCounter (testCounter + 1)
                                })
                Children =
                    [
                        str $"counter (+{testCounter})"
                    ]
            |}

    [<ReactComponent>]
    let MountButton () =
        let mounted, setMounted = Store.useState State.Atoms.Sample.mounted

        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | MountButton [ render ] mounted={mounted}")

        Button.Button
            {|
                Tooltip = Some (str "Tooltip test")
                Icon = Some (Icons.io5.IoRefreshCircle |> Icons.render, Button.IconPosition.Left)
                Props =
                    fun x ->
                        x.onClick <-
                            (fun _ ->
                                promise {
                                    Profiling.addTimestamp
                                        (fun () -> $"{nameof FsBeacon} | MountButton [ onClick ] mounted={mounted}")

                                    setMounted (not mounted)
                                })
                Children =
                    [
                        str (if mounted then "unmount" else "mount")
                    ]
            |}

    [<ReactComponent>]
    let HrefIndicator () =
        let _routeTrigger = Store.useValue Atoms.routeTrigger

        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | HrefIndicator [ render ] _routeTrigger={_routeTrigger}")

        str $"href: {Browser.Dom.window.location.href}"


    [<ReactComponent>]
    let SettingsIndicator () =
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | SettingsIndicator [ render ] ")

        Ui.box
            (fun x ->
                x.flex <- "1"
                x.whiteSpace <- "pre-wrap")
            []

    [<ReactComponent>]
    let InnerComponent () =
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | InnerComponent [ render ] ")

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

                Files ()

            //                SettingsIndicator ()
            ]


    [<ReactComponent>]
    let MessageConsumer messageIdAtom =
        let logger = Store.useValue Selectors.logger
        let deviceInfo = Store.useValue Selectors.deviceInfo
        let appState = Store.useValue (Atoms.Device.appState deviceInfo.DeviceId)
        let consumeCommands = Store.useCallbackRef (Engine.consumeCommands Messaging.appUpdate appState)
        let messageId = Store.useValue messageIdAtom
        let appMessage = Store.useValue (Atoms.Message.appMessage messageId)
        let ack, setAck = Store.useState (Atoms.Message.ack messageId)

        Profiling.addTimestamp
            (fun () ->
                $"{nameof FsBeacon} | MessageConsumer [ render ] messageId={messageId} ack={ack} appMessage={appMessage}")

        React.useEffect (
            (fun () ->
                promise {
                    match ack with
                    | Some false ->
                        match appMessage with
                        | Message.Command command ->
                            Profiling.addTimestamp
                                (fun () ->
                                    $"{nameof FsBeacon} | MessageConsumer [ render ] starting consumeCommands...")

                            let! events = consumeCommands (command |> List.singleton)

                            logger.Info
                                (fun () ->
                                    $"MessageConsumer. command processed. acked. command={command} events={events} ")

                            setAck (Some true)
                        | _ -> failwith "MessageConsumer. invalid command"

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
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | MessagesListener [ render ] ")
        let messageIdAtoms = Store.useValue State.Selectors.Sample.messageIdAtoms

        React.fragment [
            yield! messageIdAtoms |> Array.map MessageConsumer
        ]

    [<ReactComponent>]
    let HydrateSyncContainerWrapper () =
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | HydrateSyncContainerWrapper [ render ] ")
        let syncHydrateStarted = Store.useValue Atoms.Sample.syncHydrateStarted
        let syncHydrateCompleted = Store.useValue Atoms.Sample.syncHydrateCompleted

        React.fragment [
            if not syncHydrateCompleted && syncHydrateStarted then
                HydrateSyncContainer ()
        ]

    [<ReactComponent>]
    let SampleComponent () =
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | Component [ render ] ")

        let mounted = Store.useValue State.Atoms.Sample.mounted

        Ui.stack
            (fun x -> x.padding <- "15px")
            [
                HydrateCoreContainer ()

                ClearLogsButton ()

                ToggleLogsButton ()

                MountButton ()

                CounterButton ()

                if mounted then
                    HydrateSyncContainerWrapper ()
                    MessagesListener ()
                    InnerComponent ()

                DebugPanel.DebugPanel DebugPanel.DebugPanelDisplay.Inline
            ]

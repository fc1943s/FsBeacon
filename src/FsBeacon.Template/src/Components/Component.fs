namespace FsBeacon.Template.Components


open Fable.React
open Feliz
open FsJs
open FsStore
open FsStore.Bindings
open FsUi.Bindings
open FsUi.State
open FsUi.Components

module Component =
    [<ReactComponent>]
    let Component () =
        Dom.Logger.Default.Debug (fun () -> "Component.render")

        Jotai.jotaiUtils.useHydrateAtoms [|
            //            unbox Atoms.username, unbox (Some (Username "Test"))
            unbox Atoms.showDebug, unbox true
            unbox Atoms.logLevel, unbox Dom.LogLevel.Debug
            unbox Atoms.gunOptions,
            unbox (
                Model.GunOptions.Sync [|
                    Gun.GunPeer "https://localhost:49221"
                |]
            )
            unbox Atoms.hubUrl, unbox (Some "https://localhost:49211")
        |]

        let deviceInfo = Store.useValue Selectors.deviceInfo
        let gunOptions = Store.useValue Atoms.gunOptions
        let gunPeers = Store.useValue Selectors.Gun.gunPeers
        let hubUrl = Store.useValue Atoms.hubUrl
        let uiState = Store.useValue Selectors.Ui.uiState
        let sessionRestored = Store.useValue Atoms.sessionRestored
        let showDebug = Store.useValue Atoms.showDebug
        let username = Store.useValue Atoms.username

        Ui.box
            (fun x ->
                x.fontSize <- "11px"
                x.margin <- "15px")
            [
                Ui.box
                    (fun _ -> ())
                    [
                        str $"#2 {Browser.Dom.window.location.href}"
                    ]

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
                                                Username = username
                                            |}}"
                            ]

                        Button.Button
                            {|
                                Icon = Some (Icons.bi.BiPlus |> Icons.render, Button.IconPosition.Left)
                                Hint = None
                                Props = fun _ -> ()
                                Children =
                                    [
                                        str $"{Browser.Dom.window.location.port}:add_file"
                                    ]
                            |}

                        yield!
                            [
                                0 .. 2
                            ]
                            |> List.map
                                (fun i ->
                                    Ui.stack
                                        (fun x ->
                                            x.direction <- "row"
                                            x.alignItems <- "center")
                                        [
                                            Ui.box
                                                (fun _ -> ())
                                                [
                                                    str $"{Browser.Dom.window.location.port}:file[{i}]=0%%"
                                                ]
                                            Button.Button
                                                {|
                                                    Icon =
                                                        Some (Icons.bi.BiSave |> Icons.render, Button.IconPosition.Left)
                                                    Hint = None
                                                    Props = fun _ -> ()
                                                    Children =
                                                        [
                                                            str $"{Browser.Dom.window.location.port}:file[{i}]:save"
                                                        ]
                                                |}
                                            Button.Button
                                                {|
                                                    Icon =
                                                        Some (
                                                            Icons.bi.BiTrash |> Icons.render,
                                                            Button.IconPosition.Left
                                                        )
                                                    Hint = None
                                                    Props = fun _ -> ()
                                                    Children = []
                                                |}
                                        ])
                    ]


                DebugPanel.DebugPanel DebugPanel.DebugPanelDisplay.Inline
            ]

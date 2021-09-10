namespace FsBeacon.Template.Components

open FsStore
open FsCore
open Feliz
open FsJs
open FsStore.Hooks
open FsBeacon.Template.State
open FsUi.Bindings
open FsUi.Components


module SampleComponent =
    [<ReactComponent>]
    let SampleComponent () =
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | Component [ render ] ") getLocals

        let mounted = Store.useValue Atoms.Sample.mounted

        Ui.stack
            (fun x -> x.padding <- "15px")
            [
                HydrateCoreContainer.HydrateCoreContainer ()
                ClearLogsButton.ClearLogsButton ()
                ToggleLogsButton.ToggleLogsButton ()
                MountButton.MountButton ()

                if mounted then
                    EnableGunSyncButton.EnableGunSyncButton ()
                    EnableHubSyncButton.EnableHubSyncButton ()
                    DisableSyncButton.DisableSyncButton ()
                    CounterButton.CounterButton ()
                    ResetCounterButton.ResetCounterButton ()
                    InnerComponent.InnerComponent ()

                DebugPanel.DebugPanel DebugPanel.DebugPanelDisplay.Inline
            ]

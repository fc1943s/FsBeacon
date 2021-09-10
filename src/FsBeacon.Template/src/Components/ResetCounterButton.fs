namespace FsBeacon.Template.Components

open FsStore
open FsCore
open Fable.React
open Feliz
open FsJs
open FsStore.Hooks
open FsBeacon.Template.State
open FsBeacon.Template
open FsUi.Bindings
open FsUi.Components


module ResetCounterButton =
    [<ReactComponent>]
    let ResetCounterButton () =
        let setTestCounter = Store.useSetState State.Atoms.Sample.testCounter

        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | ResetCounterButton [ render ]") getLocals

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
                                        (fun () -> $"{nameof FsBeacon} | ResetCounterButton [ onClick ]")
                                        getLocals

                                    setTestCounter 0
                                })
                Children =
                    [
                        str "reset counter"
                    ]
            |}

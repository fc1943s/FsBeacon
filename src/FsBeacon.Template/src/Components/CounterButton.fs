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


module CounterButton =
    [<ReactComponent>]
    let CounterButton () =
        let testCounter, setTestCounter = Store.useState State.Atoms.Sample.testCounter

        let getLocals () =
            $"testCounter={testCounter} {getLocals ()}"

        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | CounterButton [ render ]") getLocals

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
                                        (fun () -> $"{nameof FsBeacon} | CounterButton [ onClick ]")
                                        getLocals

                                    setTestCounter (testCounter + 1)
                                })
                Children =
                    [
                        str $"counter (+{testCounter})"
                    ]
            |}

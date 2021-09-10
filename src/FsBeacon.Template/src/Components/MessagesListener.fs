namespace FsBeacon.Template.Components

open FsStore
open FsCore
open Feliz
open FsJs
open FsStore.Hooks
open FsBeacon.Template.State
open FsUi.Bindings
open FsUi.Hooks


module MessagesListener =
    [<ReactComponent>]
    let MessagesListener () =
        Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | MessagesListener [ render ] ") getLocals
        let messageIdAtoms = Store.useValue Selectors.Sample.messageIdAtoms

        React.fragment [
            yield!
                messageIdAtoms
                |> Array.map MessageConsumer.MessageConsumer
        ]

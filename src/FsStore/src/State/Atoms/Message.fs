namespace FsStore.State.Atoms

open FsCore.BaseModel
open FsStore
open FsStore.Model
open FsStore.Store


module rec Message =
    let collection = Collection (nameof Message)

    let inline messageIdIdentifier messageId =
        messageId
        |> MessageId.Value
        |> string
        |> List.singleton

    let rec ack =
        Store.atomFamilyWithSync
            FsStore.root
            collection
            (nameof ack)
            (fun (_: MessageId) -> None: bool option)
            messageIdIdentifier

    let rec appMessage =
        Store.atomFamilyWithSync
            FsStore.root
            collection
            (nameof appMessage)
            (fun (_: MessageId) -> Message<AppCommand, AppEvent>.Command (AppCommand.Init AppEngineState.Default))
            messageIdIdentifier

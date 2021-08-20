namespace FsStore.State.Atoms

open FsCore.BaseModel
open FsStore
open FsStore.Store
open FsStore.Model


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

    let rec message =
        Store.atomFamilyWithSync
            FsStore.root
            collection
            (nameof message)
            (fun (_: MessageId) -> Model.Message.None)
            messageIdIdentifier

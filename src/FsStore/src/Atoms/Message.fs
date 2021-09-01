namespace FsStore.State.Atoms

open FsCore.BaseModel
open FsStore
open FsStore.Bindings
open FsStore.Model

#nowarn "40"


module rec Message =
    let collection = Collection (nameof Message)

    let formatMessageId =
        Engine.getKeysFormatter
            (fun messageId ->
                messageId
                |> MessageId.Value
                |> string
                |> Gun.AtomKeyFragment
                |> List.singleton)

    let inline messageAtomFamilyWithAdapters atomName defaultValue =
        Atom.Primitives.atomFamily
            (fun (messageId: MessageId) ->
                Engine.createRegisteredAtomWithSubscription
                    (IndexedAtomPath (FsStore.storeRoot, collection, formatMessageId messageId, atomName))
                    defaultValue)

    let rec ack = messageAtomFamilyWithAdapters (AtomName (nameof ack)) (None: bool option)

    let rec appMessage =
        messageAtomFamilyWithAdapters
            (AtomName (nameof appMessage))
            (Message<AppCommand, AppEvent>.Command (AppCommand.Init AppEngineState.Default))

namespace FsStore.State.Atoms

open FsCore.BaseModel
open FsStore
open FsStore.Model
open Microsoft.FSharp.Core.Operators
open FsCore

#nowarn "40"


module rec File =
    let collection = Collection (nameof File)

    let formatFileId = Engine.getKeysFormatter (fun fileId -> fileId |> FileId.Value |> string |> List.singleton)


    let rec chunkCount =
        Atom.Primitives.atomFamily
            (fun fileId ->
                Engine.createRegisteredAtomWithSubscription
                    (IndexedAtomPath (FsStore.storeRoot, collection, formatFileId fileId, AtomName (nameof chunkCount)))
                    0)

    let rec chunk =
        Atom.Primitives.atomFamily
            (fun (fileId, index: int) ->
                Engine.createRegisteredAtomWithSubscription
                    (IndexedAtomPath (
                        FsStore.storeRoot,
                        collection,
                        formatFileId fileId
                        @ [
                            string index
                        ],
                        AtomName (nameof chunk)
                    ))
                    "")

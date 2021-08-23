namespace FsStore.State.Atoms

open FsCore.BaseModel
open FsStore
open FsStore.Model
open Microsoft.FSharp.Core.Operators
open FsCore
open FsStore.Bindings.Jotai

#nowarn "40"


module rec File =
    let collection = Collection (nameof File)

    let fileIdIdentifier (fileId: FileId) =
        fileId |> FileId.Value |> string |> List.singleton

    let atomFamilyWithAdapters keyIdentifierFn atomName (defaultValue: 'A) =
        Atom.Primitives.atomFamily
            (fun (key: 'TKey) ->
                Atom.createRegistered
                    (IndexedAtomPath (FsStore.storeRoot, collection, keyIdentifierFn key, atomName))
                    (AtomType.Atom defaultValue)
                |> Atom.enableAdapters)

    let fileAtomFamilyWithAdapters = atomFamilyWithAdapters fileIdIdentifier

    let rec chunkCount = fileAtomFamilyWithAdapters (AtomName (nameof chunkCount)) 0

    let rec chunk =
        atomFamilyWithAdapters
            (fun (fileId, index: int) ->
                fileIdIdentifier fileId
                @ [
                    string index
                ])
            (AtomName (nameof chunk))
            ""

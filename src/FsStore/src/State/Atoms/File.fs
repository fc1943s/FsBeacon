namespace FsStore.State.Atoms

open FsCore.BaseModel
open FsStore

#nowarn "40"


module rec File =
    let collection = Collection (nameof File)

    let fileIdIdentifier (fileId: FileId) =
        fileId |> FileId.Value |> string |> List.singleton

    let rec chunkCount =
        Store.atomFamilyWithSync FsStore.root collection (nameof chunkCount) (fun (_: FileId) -> 0) fileIdIdentifier

    let rec chunk =
        Store.atomFamilyWithSync
            FsStore.root
            collection
            (nameof chunk)
            (fun (_: FileId, _: int) -> "")
            (fun (fileId: FileId, index: int) ->
                fileIdIdentifier fileId
                @ [
                    string index
                ])

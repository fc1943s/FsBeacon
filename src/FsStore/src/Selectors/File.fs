namespace FsStore.State.Selectors

open FsCore.BaseModel
open FsStore.State
open FsJs
open FsStore
open FsStore.Bindings.Jotai
open FsStore.Model
open Microsoft.FSharp.Core.Operators

#nowarn "40"


module rec File =
    let fileReadSelectorFamily atomName fn =
        Atom.Primitives.atomFamily
            (fun (fileId: FileId) ->
                Atom.createRegistered
                    (IndexedAtomPath (
                        FsStore.storeRoot,
                        Atoms.File.collection,
                        Atoms.File.formatFileId fileId,
                        atomName
                    ))
                    (AtomType.ReadSelector (fn fileId)))

    let rec hexString =
        fileReadSelectorFamily
            (AtomName (nameof hexString))
            (fun fileId getter ->
                let logger = Atom.get getter Selectors.logger
                let chunkCount = Atom.get getter (Atoms.File.chunkCount fileId)

                match chunkCount with
                | 0 -> None
                | _ ->
                    let chunks =
                        [|
                            0 .. chunkCount - 1
                        |]
                        |> Array.map (fun i -> Atoms.File.chunk (fileId, i))
                        |> Atom.waitForAll
                        |> Atom.get getter

                    if chunks |> Array.exists (String.length >> (=) 0) then
                        logger.Debug
                            (fun () ->
                                $"File.blob incomplete blob. skipping
chunkCount={chunkCount} chunks.Length={chunks.Length}
chunks.[0].Length={if chunks.Length = 0 then unbox null else chunks.[0].Length} ")

                        None
                    else
                        logger.Debug
                            (fun () ->
                                $"File.blob chunkCount={chunkCount}
chunks.Length={chunks.Length}
chunks.[0].Length={if chunks.Length = 0 then unbox null else chunks.[0].Length} ")

                        match chunks |> String.concat "" with
                        | "" -> None
                        | chunks -> Some chunks)

    let rec byteArray =
        fileReadSelectorFamily
            (AtomName (nameof byteArray))
            (fun fileId getter ->
                let hexString = Atom.get getter (hexString fileId)
                hexString |> Option.map Js.hexStringToByteArray)

    let rec progress =
        fileReadSelectorFamily
            (AtomName (nameof progress))
            (fun fileId getter ->
                let chunkCount = Atom.get getter (Atoms.File.chunkCount fileId)

                match chunkCount with
                | 0 -> 0
                | _ ->
                    let chunkLengthArray =
                        [|
                            0 .. chunkCount - 1
                        |]
                        |> Array.map (fun i -> Atoms.File.chunk (fileId, i))
                        |> Atom.waitForAll
                        |> Atom.get getter
                        |> Array.map (fun chunk -> chunk.Length)

                    let completedChunkCount =
                        chunkLengthArray
                        |> Array.filter (fun len -> len > 0)
                        |> Array.length

                    let progress = 100 / chunkCount * completedChunkCount

                    Profiling.addTimestamp
                        (fun () ->
                            $"File.progress
                                                size(bytes)={chunkLengthArray |> Array.sum}
                                                size(kb)={(chunkLengthArray |> Array.sum) / 1024}
                                                chunkCount={chunkCount}
                                                completedChunkCount={completedChunkCount}
                                                progress={progress} ")

                    progress)

    let rec blob =
        fileReadSelectorFamily
            (AtomName (nameof blob))
            (fun fileId getter ->
                let byteArray = Atom.get getter (byteArray fileId)

                byteArray
                |> Option.map (Js.byteArrayToBlob "image/png"))

    let rec objectUrl =
        fileReadSelectorFamily
            (AtomName (nameof objectUrl))
            (fun fileId getter ->
                let blob = Atom.get getter (blob fileId)
                blob |> Option.map Browser.Url.URL.createObjectURL)

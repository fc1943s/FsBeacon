namespace FsStore.Hooks

open FsCore.Model
open FsJs
open FsStore
open System
open FsStore.Model
open FsStore.State


module Hydrate =
    let fileChunkSize = 12800

    let inline hydrateFile setter (atomScope: AtomScope, hexString: string) =
        let chunkCount = int (Math.Ceiling (float hexString.Length / float fileChunkSize))

        let chunks =
            Js.chunkString
                hexString
                {|
                    size = fileChunkSize
                    unicodeAware = false
                |}

        Dom.Logger.Default.Debug
            (fun () ->
                $"hydrateFile.
        base64.Length={hexString.Length}
        chunkCount={chunkCount}
        chunks.[0].Length={chunks.[0].Length}
        ")

        let fileId = FileId.NewId ()
        Store.set setter (Atoms.File.chunkCount fileId) chunkCount

        chunks
        |> Array.iteri (fun i chunk -> Store.scopedSet setter atomScope (Atoms.File.chunk, (fileId, i), chunk))

        fileId

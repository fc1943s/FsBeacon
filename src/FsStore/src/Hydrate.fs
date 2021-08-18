namespace FsStore.Hooks

open FsCore
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

        match hexString, chunkCount with
        | String.InvalidString, _
        | _, 0 ->
            Dom.logDebug
                (fun () -> $"hydrateFile. invalid hexString.Length={hexString.Length} chunkCount={chunkCount} ")

            None
        | _ ->
            let chunks =
                Js.chunkString
                    hexString
                    {|
                        size = fileChunkSize
                        unicodeAware = false
                    |}

            Dom.logDebug
                (fun () ->
                    $"hydrateFile.
            hexString.Length={hexString.Length}
            chunkCount={chunkCount}
            chunks.Length={chunks.Length}
            chunks.[0].Length={if chunks.Length = 0 then unbox null else chunks.[0].Length}
            ")

            if chunks.Length = chunkCount then
                let fileId = FileId.NewId ()
                Store.set setter (Atoms.File.chunkCount fileId) chunkCount

                chunks
                |> Array.iteri (fun i chunk -> Store.scopedSet setter atomScope (Atoms.File.chunk, (fileId, i), chunk))

                Some fileId
            else
                None

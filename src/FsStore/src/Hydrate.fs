namespace FsStore

open FsCore
open FsCore.BaseModel
open FsJs
open FsStore
open System
open FsStore.Model
open FsStore.State


module Hydrate =
    let fileChunkSize = 12800

    let inline hydrateAppMessage setter message =
        let messageId = MessageId.NewId ()
        Atom.set setter (State.Atoms.Message.appMessage messageId) message
        Atom.set setter (State.Atoms.Message.ack messageId) (Some false)
        messageId

    let inline hydrateFile setter (atomScope: AtomScope, hexString: string) =
        let chunkCount = int (Math.Ceiling (float hexString.Length / float fileChunkSize))

        match hexString, chunkCount with
        | String.Invalid, _
        | _, 0 ->
            Logger.logDebug
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

            Logger.logDebug
                (fun () ->
                    $"hydrateFile.
            hexString.Length={hexString.Length}
            chunkCount={chunkCount}
            chunks.Length={chunks.Length}
            chunks.[0].Length={if chunks.Length = 0 then unbox null else chunks.[0].Length}
            ")

            if chunks.Length = chunkCount then
                let fileId = FileId.NewId ()
                Atom.set setter (Atoms.File.chunkCount fileId) chunkCount

                chunks
                |> Array.iteri (fun i chunk -> Store.scopedSet setter atomScope (Atoms.File.chunk, (fileId, i), chunk))

                Some fileId
            else
                None

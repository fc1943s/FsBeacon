namespace FsStore.State.Selectors

open FsCore.BaseModel
open FsStore.State
open FsJs
open FsStore

#nowarn "40"


module rec File =
    let rec hexString =
        Store.readSelectorFamily
            FsStore.root
            (nameof byteArray)
            (fun (fileId: FileId) getter ->
                let logger = Store.value getter Selectors.logger
                let chunkCount = Store.value getter (Atoms.File.chunkCount fileId)

                match chunkCount with
                | 0 -> None
                | _ ->
                    let chunks =
                        [|
                            0 .. chunkCount - 1
                        |]
                        |> Array.map (fun i -> Atoms.File.chunk (fileId, i))
                        |> Store.waitForAll
                        |> Store.value getter

                    if chunks |> Array.exists (String.length >> (=) 0) then
                        logger.Debug
                            (fun () ->
                                $"File.blob
                                        incomplete blob. skipping
                                    chunkCount={chunkCount}
                                    chunks.Length={chunks.Length}
                                    chunks.[0].Length={if chunks.Length = 0 then unbox null else chunks.[0].Length}
                                    ")

                        None
                    else
                        logger.Debug
                            (fun () ->
                                $"File.blob
                                    chunkCount={chunkCount}
                                    chunks.Length={chunks.Length}
                                    chunks.[0].Length={if chunks.Length = 0 then unbox null else chunks.[0].Length}
                                    ")

                        match chunks |> String.concat "" with
                        | "" -> None
                        | chunks -> Some chunks)

    let rec byteArray =
        Store.readSelectorFamily
            FsStore.root
            (nameof byteArray)
            (fun (fileId: FileId) getter ->
                let hexString = Store.value getter (hexString fileId)
                hexString |> Option.map Js.hexStringToByteArray)

    let rec progress =
        Store.readSelectorFamily
            FsStore.root
            (nameof progress)
            (fun (fileId: FileId) getter ->
                let logger = Store.value getter Selectors.logger
                let chunkCount = Store.value getter (Atoms.File.chunkCount fileId)

                match chunkCount with
                | 0 -> 0
                | _ ->
                    let completedChunkCount =
                        [|
                            0 .. chunkCount - 1
                        |]
                        |> Array.map (fun i -> Atoms.File.chunk (fileId, i))
                        |> Store.waitForAll
                        |> Store.value getter
                        |> Array.filter (fun chunk -> chunk.Length > 0)
                        |> Array.length

                    let progress = 100 / chunkCount * completedChunkCount

                    logger.Debug
                        (fun () ->
                            $"File.progress
                                    chunkCount={chunkCount}
                                    completedChunkCount={completedChunkCount}
                                    progress={progress} ")

                    progress)


    let rec blob =
        Store.readSelectorFamily
            FsStore.root
            (nameof blob)
            (fun (fileId: FileId) getter ->
                let byteArray = Store.value getter (byteArray fileId)

                byteArray
                |> Option.map (Js.byteArrayToBlob "image/png"))


    let rec objectUrl =
        Store.readSelectorFamily
            FsStore.root
            (nameof objectUrl)
            (fun (fileId: FileId) getter ->
                let blob = Store.value getter (blob fileId)
                blob |> Option.map Browser.Url.URL.createObjectURL)

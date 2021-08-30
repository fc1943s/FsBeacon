namespace FsBeacon.Template

open FsStore.Bindings.Jotai
open FsCore
open FsCore.BaseModel
open FsStore
open FsStore.Model
open FsStore.State
open FsUi.State

#nowarn "40"


module State =
    module FsBeacon =
        let storeRoot = StoreRoot (nameof FsBeacon)


    module rec Host =
        [<RequireQualifiedAccess>]
        type AccordionType = | HostComponent


    module Atoms =

        module rec Host =
            let collection = Collection (nameof Host)

            let rec accordionHiddenFlag =
                Atom.Primitives.atomFamily
                    (fun (accordionType: Host.AccordionType) ->
                        Atom.createRegistered
                            (IndexedAtomPath (
                                FsBeacon.storeRoot,
                                collection,
                                accordionType |> string |> List.singleton,
                                (AtomName (nameof accordionHiddenFlag))
                            ))
                            (AtomType.Atom ([||]: string [])))


        module Sample =
            let rec syncHydrateStarted =
                Atom.createRegistered
                    (RootAtomPath (FsBeacon.storeRoot, AtomName (nameof syncHydrateStarted)))
                    (AtomType.Atom false)

            let rec syncHydrateCompleted =
                Atom.createRegistered
                    (RootAtomPath (FsBeacon.storeRoot, AtomName (nameof syncHydrateCompleted)))
                    (AtomType.Atom false)

            let rec mounted =
                Atom.createRegistered
                    (RootAtomPath (FsBeacon.storeRoot, AtomName (nameof mounted)))
                    (AtomType.Atom false)


            module Operation =
                [<RequireQualifiedAccess>]
                type Operation<'T> =
                    | Waiting
                    | Ok of 'T
                    | Progress of int
                    | Error of exn

                let asyncOperationAtom fn =
                    let operationAtom = Atom.Primitives.atom Operation.Waiting

                    let asyncWriteAtom =
                        Atom.Primitives.asyncSelector
                            (fun _ -> promise { return () })
                            (fun _ setter _ ->
                                promise {
                                    Atom.set setter operationAtom (Operation.Progress 0)

                                    try
                                        let! result = fn ()
                                        Atom.set setter operationAtom (Operation.Ok result)
                                    with
                                    | ex -> Atom.set setter operationAtom (Operation.Error ex)
                                })

                    Atom.Primitives.selector
                        (fun getter -> Atom.get getter operationAtom)
                        (fun _ setter _ -> Atom.set setter asyncWriteAtom ())

                let createFileDownloadAtom url =
                    asyncOperationAtom
                        (fun () ->
                            promise {
                                do! Promise.sleep 1000
                                return $"downloaded after 1s. url: {url}"
                            })

            let rec upvote = Atoms.Join.createJoinAtom (nameof upvote)
            let rec downvote = Atoms.Join.createJoinAtom (nameof downvote)


    module Selectors =
        module Sample =
            let fileIdAtoms = Engine.subscribeFamilyKey Atoms.File.chunkCount
            let messageIdAtoms = Engine.subscribeFamilyKey Atoms.Message.ack

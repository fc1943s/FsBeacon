namespace FsBeacon.Template

open FsStore.Bindings.Jotai
open FsStore.Store
open System
open FsCore
open FsCore.BaseModel
open FsStore
open FsStore.Bindings
open FsStore.Model
open FsStore.State
open FsUi.State

#nowarn "40"


module State =
    module FsBeacon =
        let storeRoot = StoreRoot (nameof FsBeacon)


    [<RequireQualifiedAccess>]
    type AccordionType = | HostComponent


    module Atoms =

        module rec Host =
            let collection = Collection (nameof Host)

            let rec accordionHiddenFlag =
                Atom.Primitives.atomFamily
                    (fun (accordionType: AccordionType) ->
                        Atom.createRegistered
                            (IndexedAtomPath (
                                FsBeacon.storeRoot,
                                collection,
                                accordionType |> string |> List.singleton,
                                (AtomName (nameof accordionHiddenFlag))
                            ))
                            (AtomType.Atom ([||]: string [])))


        let rec syncHydrateStarted =
            Atom.createRegistered
                (RootAtomPath (FsBeacon.storeRoot, AtomName (nameof syncHydrateStarted)))
                (AtomType.Atom false)

        let rec syncHydrateCompleted =
            Atom.createRegistered
                (RootAtomPath (FsBeacon.storeRoot, AtomName (nameof syncHydrateCompleted)))
                (AtomType.Atom false)

        let rec signInStarted =
            Atom.createRegistered
                (RootAtomPath (FsBeacon.storeRoot, AtomName (nameof signInStarted)))
                (AtomType.Atom false)

        let rec mounted =
            Atom.createRegistered (RootAtomPath (FsBeacon.storeRoot, AtomName (nameof mounted))) (AtomType.Atom false)




    module Selectors =
        let rec asyncFileIdAtoms =
            Store.selectAtomSyncKeys (CollectionAtomPath (FsStore.storeRoot, Atoms.File.collection)) (Guid >> FileId)

        let rec asyncMessageIdAtoms =
            Store.selectAtomSyncKeys
                (CollectionAtomPath (FsStore.storeRoot, Atoms.Message.collection))
                (Guid >> MessageId)

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

            let rec testCounter =
                Engine.createRegisteredAtomWithSubscriptionStorage
        //        Atom.createRegisteredWithStorage
                    (RootAtomPath (FsBeacon.storeRoot, AtomName (nameof testCounter)))
                    0


    module Selectors =
        module Sample =
            let fileIdAtoms = Engine.subscribeCollection FsStore.storeRoot Atoms.File.collection FileId
            let messageIdAtoms = Engine.subscribeCollection FsStore.storeRoot Atoms.Message.collection MessageId

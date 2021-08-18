namespace FsBeacon.Template


open System
open FsCore
open FsCore.Model
open FsStore
open FsStore.Bindings
open FsStore.Model
open FsStore.State
open FsUi.State


module State =
    module FsBeacon =
        let root = StoreRoot (nameof FsBeacon)


    [<RequireQualifiedAccess>]
    type AccordionType = | HostComponent


    module Atoms =

        module rec Host =
            let collection = Collection (nameof Host)

            let rec accordionHiddenFlag =
                Store.atomFamily
                    FsBeacon.root
                    collection
                    (nameof accordionHiddenFlag)
                    (fun (_: AccordionType) -> [||]: string [])
                    (string >> List.singleton)

        let rec hydrateStarted = Store.atom FsBeacon.root (nameof hydrateStarted) false
        let rec hydrateCompleted = Store.atom FsBeacon.root (nameof hydrateCompleted) false
        let rec signInStarted = Store.atom FsBeacon.root (nameof signInStarted) false

        module File =
            let rec pub =
                Store.atomFamilyWithSync
                    FsStore.root
                    Atoms.File.collection
                    (nameof pub)
                    (fun (_: FileId) -> None: Gun.Pub option)
                    Atoms.File.fileIdIdentifier


    module Selectors =
        let rec asyncFileIdAtoms =
            Store.selectAtomSyncKeys
                FsStore.root
                (nameof asyncFileIdAtoms)
                Atoms.File.pub
                (FileId Guid.Empty)
                (Guid >> FileId)

        let rec asyncMessageIdAtoms =
            Store.selectAtomSyncKeys
                FsBeacon.root
                (nameof asyncMessageIdAtoms)
                Atoms.Message.ack
                (MessageId Guid.Empty)
                (Guid >> MessageId)

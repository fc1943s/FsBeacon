namespace FsBeacon.Template


open System
open FsCore.Model
open FsJs
open FsStore
open FsStore.State
open FsStore.Bindings
open FsUi.State


module State =
    module FsBeacon =
        let root = StoreRoot (nameof FsBeacon)
    //    let rec asyncFileIdAtoms =
//        Store.selectAtomSyncKeys
//            FsBeacon.root
//            (nameof asyncFileIdAtoms)
//            Atoms.File.chunkCount
//            (FileId Guid.Empty)
//            (Guid >> FileId)

    [<RequireQualifiedAccess>]
    type AccordionType = | Host

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
        let rec signInStarted = Store.atom FsBeacon.root (nameof signInStarted) false

        module Device =
            let rec fileId: (DeviceId -> Jotai.Atom<FileId>) =
                Store.atomFamilyWithSync
                    FsBeacon.root
                    Atoms.Device.collection
                    (nameof fileId)
                    (fun (_: DeviceId) -> FileId Guid.Empty)
                    Atoms.Device.deviceIdIdentifier

    module Selectors =
        let rec asyncDeviceIdAtoms =
            Store.selectAtomSyncKeys
                FsBeacon.root
                (nameof asyncDeviceIdAtoms)
                Atoms.Device.fileId
                Dom.deviceInfo.DeviceId
                (Guid >> DeviceId)

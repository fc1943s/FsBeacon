namespace FsStore.State

open FsStore.Bindings.Jotai
open FsStore.Model
open FsStore
open Microsoft.FSharp.Core.Operators
open FsJs


[<AutoOpen>]
module AtomsMagic =
    module Atoms =
        let rec logLevel =
            Atom.createRegisteredWithStorage
                (RootAtomPath (FsStore.storeRoot, AtomName (nameof logLevel)))
                Logger.DEFAULT_LOG_LEVEL

        let rec showDebug =
            Atom.createRegisteredWithStorage
                (RootAtomPath (FsStore.storeRoot, AtomName (nameof showDebug)))
                Dom.deviceInfo.IsTesting

        let rec gunOptions =
            Atom.createRegisteredWithStorage
                (RootAtomPath (FsStore.storeRoot, AtomName (nameof gunOptions)))
                (GunOptions.Sync [||])

        let rec hubUrl =
            Atom.createRegisteredWithStorage
                (RootAtomPath (FsStore.storeRoot, AtomName (nameof hubUrl)))
                (None: string option)

        let rec sessionRestored =
            Atom.createRegistered
                (RootAtomPath (FsStore.storeRoot, AtomName (nameof sessionRestored)))
                (AtomType.Atom false)

        let rec gunTrigger =
            Atom.createRegistered (RootAtomPath (FsStore.storeRoot, AtomName (nameof gunTrigger))) (AtomType.Atom 0)

        let rec hubTrigger =
            Atom.createRegistered (RootAtomPath (FsStore.storeRoot, AtomName (nameof hubTrigger))) (AtomType.Atom 0)

        let rec routeTrigger =
            Atom.createRegistered (RootAtomPath (FsStore.storeRoot, AtomName (nameof routeTrigger))) (AtomType.Atom 0)

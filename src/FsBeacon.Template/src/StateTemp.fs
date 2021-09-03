namespace FsBeacon.Template

open FsStore
open FsStore.Model

[<AutoOpen>]
module StateTempMagic =
    module State =
        module Atoms =
            module Sample =
                let rec testCounter =
                    Engine.createAtomWithSubscriptionStorage
                        (RootAtomPath (State.FsBeacon.storeRoot, AtomName (nameof testCounter)))
                        0

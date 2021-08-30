namespace FsBeacon.Template

open FsStore
open FsStore.Model

[<AutoOpen>]
module StateTempMagic =
    module State =
        module Atoms =
            module Sample =
                let rec testCounter =
                    Engine.createRegisteredAtomWithSubscriptionStorage
                        //        Atom.createRegisteredWithStorage
                        (RootAtomPath (State.FsBeacon.storeRoot, AtomName (nameof testCounter)))
                        0

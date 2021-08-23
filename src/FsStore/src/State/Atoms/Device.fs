namespace FsStore.State.Atoms

open FsCore.BaseModel
open FsStore
open FsStore.Bindings.Jotai
open FsStore.Model

#nowarn "40"


module rec Device =
    let collection = Collection (nameof Device)

    let inline deviceIdIdentifier deviceId =
        deviceId
        |> DeviceId.Value
        |> string
        |> List.singleton


    let inline deviceAtomFamilyWithAdapters atomName defaultValue =
        Atom.Primitives.atomFamily
            (fun (deviceId: DeviceId) ->
                Atom.createRegistered
                    (IndexedAtomPath (FsStore.storeRoot, collection, deviceIdIdentifier deviceId, atomName))
                    (AtomType.Atom defaultValue)
                |> Atom.enableAdapters)

    let rec devicePing = deviceAtomFamilyWithAdapters (AtomName (nameof devicePing)) (Ping "0")

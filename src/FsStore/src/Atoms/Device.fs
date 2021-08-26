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

    let rec devicePing =
        Atom.Primitives.atomFamily
            (fun (deviceId: DeviceId) ->
                Engine.createRegisteredAtomWithSubscription
                    (IndexedAtomPath (
                        FsStore.storeRoot,
                        collection,
                        deviceIdIdentifier deviceId,
                        AtomName (nameof devicePing)
                    ))
                    (Ping "0"))

    let rec appState =
        Atom.Primitives.atomFamily
            (fun (deviceId: DeviceId) ->
                Atom.createRegistered
                    (IndexedAtomPath (
                        FsStore.storeRoot,
                        collection,
                        deviceIdIdentifier deviceId,
                        AtomName (nameof appState)
                    ))
                    (AtomType.Atom AppEngineState.Default))

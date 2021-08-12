namespace FsStore.State.Atoms

open FsCore.Model
open FsStore


module rec Device =
    let collection = Collection (nameof Device)

    let inline deviceIdIdentifier deviceId =
        deviceId
        |> DeviceId.Value
        |> string
        |> List.singleton

    let inline atomFamilyWithSync name defaultValueFn =
        Store.atomFamilyWithSync FsStore.root collection name defaultValueFn deviceIdIdentifier

    let rec devicePing = atomFamilyWithSync (nameof devicePing) (fun (_: DeviceId) -> Ping "0")

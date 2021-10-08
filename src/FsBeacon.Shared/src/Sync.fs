namespace FsBeacon.Shared


module Sync =
    [<RequireQualifiedAccess>]
    type Request =
        | Connect of alias: string
        | Set of alias: string * atomPath: string * value: string
        | Get of alias: string * atomPath: string
        | Keys of alias: string * collectionPath: string

    [<RequireQualifiedAccess>]
    type Response =
        | ConnectResult
        | SetResult of ok: bool
        | GetResult of value: string option
        | GetStream of alias: string * atomPath: string * value: string option
        | KeysResult of keys: string []
        | KeysStream of alias: string * collectionPath: string * updatedKeys: string [] * removedKeys: string []

//    let endpoint = $"/{nameof FsBeacon}"
    let endpoint = "/fsbeacon"

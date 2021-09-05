namespace FsBeacon.Shared


module Sync =
    [<RequireQualifiedAccess>]
    type Request =
        | Connect of alias: string
        | Set of alias: string * atomPath: string * value: string
        | Get of alias: string * atomPath: string
        | Filter of alias: string * atomPath: string

    [<RequireQualifiedAccess>]
    type Response =
        | ConnectResult
        | SetResult of ok: bool
        | GetResult of value: string option
        | GetStream of alias: string * atomPath: string * value: string option
        | FilterResult of keys: string []
        | FilterStream of alias: string * atomPath: string * keys: string []

    let endpoint = "/fsbeacon"

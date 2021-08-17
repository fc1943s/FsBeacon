namespace FsCore

open System


module Model =
    type Username = Username of alias: string
    type Color = Color of hex: string
    type StoreRoot = StoreRoot of name: string
    type Collection = Collection of collection: string
    type FileId = FileId of guid: Guid
    type DeviceId = DeviceId of guid: Guid
    type Ping = Ping of ticksText: string

    type Username with
        static member inline Value value =
            try
                match value with
                | Username username -> Some username
            with
            | ex ->
                eprintfn $"Username.Value error value={value} ex={ex}"
                None

        static member inline ValueOrDefault = Username.Value >> Option.defaultValue ""

    type StoreRoot with
        static member inline Value (StoreRoot name) = name

    and Collection with
        static member inline Value (Collection collection) = collection

    and Color with
        static member inline Value value =
            match value |> Option.ofObjUnbox with
            | Some (Color hex) -> Some hex
            | _ -> None

        static member inline Default = Color "#000000"

        static member inline ValueOrDefault value =
            value
            |> Color.Value
            |> Option.defaultValue (Color.Default |> Color.Value |> Option.get)

    type FileId with
        static member inline NewId () = FileId (Guid.newTicksGuid ())
        static member inline Value (FileId guid) = guid


    type DeviceId with
        static member inline NewId () = DeviceId (Guid.newTicksGuid ())
        static member inline Value (DeviceId guid) = guid


    type Ping with
        static member inline Value (Ping ticks) = int64 ticks

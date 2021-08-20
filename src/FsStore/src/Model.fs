namespace FsStore

open Fable.Core
open FsCore
open FsCore.BaseModel
open FsStore.Bindings

module FsStore =
    let root = StoreRoot (nameof FsStore)

module Model =
    type Atom<'T> = Jotai.Atom<'T>
    type AtomScope = Jotai.AtomScope
    type GetFn = Jotai.GetFn
    type SetFn = Jotai.SetFn

    //    [<Erase; RequireQualifiedAccess>]
    [<RequireQualifiedAccess>]
    type InputScope<'TValue> =
        | Current
        | Temp of Gun.Serializer<'TValue>


    [<RequireQualifiedAccess>]
    type AtomReference<'T> =
        | Atom of Atom<'T>
        | Path of string

    type InputAtom<'T> = InputAtom of atomPath: AtomReference<'T>

    type AtomField<'TValue67> = AtomField of Atom<'TValue67> option * Atom<string> option

    [<RequireQualifiedAccess>]
    type GunOptions =
        | Minimal
        | Sync of Gun.GunPeer []


    [<Erase>]
    type AtomPath =
        | AtomPath of atomPath: string
        static member inline Value (AtomPath atomPath) = atomPath
        static member inline AtomKey _atomPath = AtomPath (failwith "invalid")


    //    let inline splitAtomPath (AtomPath atomPath) =
//        let matches =
//            (JSe.RegExp @"(.*?)\/([\w-]{36})\/\w+.*?")
//                .Match atomPath
//            |> Option.ofObj
//            |> Option.defaultValue Seq.empty
//            |> Seq.toList
//
//        match matches with
//        | _match :: root :: guid :: _key -> Some (root, guid)
//        | _ -> None

    //        let tryTestKey table key =
//            let result = Regex.Match (key, $"^.*?/{table}/([a-fA-F0-9\\-]{{36}})")
//            if result.Groups.Count = 2 then Some result.Groups.[1].Value else None

    //            [
//                yield atomKey.StoreRoot |> StoreRoot.Value
//                match atomKey.Collection with
//                | Some collection -> yield collection |> Collection.Value
//                | None -> ()
//                yield! atomKey.Keys
//                yield atomKey.Name
//            ]
//            |> String.concat "/"
//            |> AtomPath


    type AtomKey =
        {
            StoreRoot: StoreRoot
            Collection: Collection option
            Keys: string list
            Name: string
        }


    type InputScope<'TValue> with
        static member inline AtomScope<'TValue> (inputScope: InputScope<'TValue> option) =
            match inputScope with
            | Some (InputScope.Temp _) -> AtomScope.Temp
            | _ -> AtomScope.Current

    type AtomKey with
        static member AtomPath atomKey =
            [
                yield atomKey.StoreRoot |> StoreRoot.Value
                match atomKey.Collection with
                | Some collection -> yield collection |> Collection.Value
                | None -> ()
                yield! atomKey.Keys
                yield atomKey.Name
            ]
            |> String.concat "/"
            |> AtomPath


    [<RequireQualifiedAccess>]
    type Message =
        | None
        | Command of command: Command
        | Event

    and [<RequireQualifiedAccess>] Command =
        | KeySignIn of keys: Gun.GunKeys
        | Set of key: string * value: string


    type MessageId = MessageId of TicksGuid

    type MessageId with
        static member inline NewId () = MessageId (Guid.newTicksGuid ())
        static member inline Value (MessageId guid) = guid


    type SubscriptionId = SubscriptionId of TicksGuid

    type SubscriptionId with
        static member inline NewId () = SubscriptionId (Guid.newTicksGuid ())
        static member inline Value (SubscriptionId guid) = guid

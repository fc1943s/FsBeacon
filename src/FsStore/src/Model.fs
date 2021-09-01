namespace FsStore

open Fable.Core
open FsCore
open FsCore.BaseModel
open FsStore.Bindings
open FsStore.Bindings.Gun


module FsStore =
    let storeRoot = StoreRoot (nameof FsStore)


module Model =


    [<RequireQualifiedAccess>]
    type AtomScope =
        | Current
        | Temp

    type AtomConfig<'A> = Jotai.AtomConfig<'A>

    type Getter<'A> = Jotai.Getter<'A>
    type Setter<'A> = Jotai.Setter<'A>


    type StoreAtomPath =
        | RootAtomPath of storeRoot: StoreRoot * name: AtomName
        | CollectionAtomPath of storeRoot: StoreRoot * collection: Collection
        | IndexedAtomPath of
            storeRoot: StoreRoot *
            collection: Collection *
            keys: Gun.AtomKeyFragment list *
            name: AtomName

    and AtomName = AtomName of string

    [<RequireQualifiedAccess>]
    type AtomReference<'T> =
        | Atom of Jotai.AtomConfig<'T>
        | Path of StoreAtomPath

    type InputAtom<'T> = InputAtom of atomPath: AtomReference<'T>

    type AtomField<'A> = AtomField of Jotai.AtomConfig<'A> option * Jotai.AtomConfig<string> option

    [<RequireQualifiedAccess>]
    type GunOptions =
        | Minimal
        | Sync of Gun.GunPeer []


    [<Erase>]
    type AtomPath = AtomPath of atomPath: string

    //    [<Erase; RequireQualifiedAccess>]
    [<RequireQualifiedAccess>]
    type InputScope<'A> =
        | Current
        | Temp of Serializer<'A>


    [<StructuralComparison; StructuralEquality; RequireQualifiedAccess>]
    type AppCommand =
        | Init of state: AppEngineState
        | SignInPair of keys: GunKeys

    and AppEngineState = { Adapters: unit list }

    [<RequireQualifiedAccess>]
    type AppEvent =
        | UserSignedIn
        | AdapterRegistered
        | Error of error: string

    [<RequireQualifiedAccess>]
    type AtomCommand =
        | Init of state: AtomEngineState
        | Subscribe
        | Unsubscribe

    and AtomEngineState = { Adapters: (unit -> unit) list }

    [<RequireQualifiedAccess>]
    type AtomEvent =
        | Subscribed
        | Unsubscribed
        | Error of error: string

    type SubscriptionId = SubscriptionId of TicksGuid
    type MessageId = MessageId of TicksGuid


    [<RequireQualifiedAccess>]
    type Message<'TCommand, 'TEvent> =
        | Command of command: 'TCommand
        | Event of event: 'TEvent

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

    type AtomName with
        static member inline Value (AtomName name) = name

    type AtomPath with
        static member inline Value (AtomPath atomPath) = atomPath
        static member inline AtomKey _atomPath = AtomPath (failwith "invalid")

    type AppEngineState with
        static member inline Default = { Adapters = [] }

    type AtomEngineState with
        static member inline Default = { Adapters = [] }

    type MessageId with
        static member inline NewId () = MessageId (Guid.newTicksGuid ())
        static member inline Value (MessageId guid) = guid

    type SubscriptionId with
        static member inline NewId () = SubscriptionId (Guid.newTicksGuid ())
        static member inline Value (SubscriptionId guid) = guid


    type InputScope<'TValue> with
        static member inline AtomScope<'TValue> (inputScope: InputScope<'TValue> option) =
            match inputScope with
            | Some (InputScope.Temp _) -> AtomScope.Temp
            | _ -> AtomScope.Current

    type StoreAtomPath with
        static member AtomPath storeAtomPath =
            let storeRoot, collection, keys, name =
                match storeAtomPath with
                | RootAtomPath (storeRoot, name) -> storeRoot, None, [], Some name
                | CollectionAtomPath (storeRoot, collection) -> storeRoot, Some collection, [], None
                | IndexedAtomPath (storeRoot, collection, keys, name) -> storeRoot, Some collection, keys, Some name

            [
                yield storeRoot |> StoreRoot.Value
                match collection with
                | Some collection -> yield collection |> Collection.Value
                | None -> ()
                yield! keys |> List.map AtomKeyFragment.Value
                match name with
                | Some name -> yield name |> AtomName.Value
                | None -> ()
            ]
            |> String.concat "/"
            |> AtomPath

        static member inline CollectionPath storeAtomPath =
            match storeAtomPath with
            | RootAtomPath _ -> None
            | CollectionAtomPath (storeRoot, collection) -> Some (storeRoot, collection)
            | IndexedAtomPath (storeRoot, collection, _, _) -> Some (storeRoot, collection)

        static member inline Keys storeAtomPath =
            match storeAtomPath with
            | IndexedAtomPath (_, _, keys, _) -> keys |> List.toArray |> Some
            | _ -> None

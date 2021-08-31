namespace FsStore

open FsCore
open System.Collections.Generic
open Fable.Core
open FsStore.Bindings
open FsStore.Bindings.Jotai
open FsStore.Model
open Microsoft.FSharp.Core.Operators
open FsJs
open Fable.Core.JsInterop

#nowarn "40"




module Atom =
    [<RequireQualifiedAccess>]
    type AdapterType =
        | Memory
        | Gun
        | Hub

    [<RequireQualifiedAccess>]
    type AdapterOptions =
        | Memory
        | Gun of gunPeers: Gun.GunPeer [] * alias: Gun.Alias
        | Hub of hubUrl: string * alias: Gun.Alias

    //    [<RequireQualifiedAccess>]
//    type AdapterValue<'T> =
//        | Memory of 'T
//        | Gun of 'T
//        | Hub of 'T
//
    type AtomInternalKey = AtomInternalKey of key: string

    let private atomPathMap = Dictionary<StoreAtomPath, AtomConfig<obj>> ()
    let private atomIdMap = Dictionary<AtomInternalKey, StoreAtomPath> ()

    let rec globalAtomPathMap =
        Dom.Global.register (nameof globalAtomPathMap) (Dictionary<StoreAtomPath, AtomConfig<obj>> ())

    let rec globalAtomIdMap = Dom.Global.register (nameof globalAtomIdMap) atomIdMap


    let inline get<'A> (getter: Getter<obj>) (atom: AtomConfig<'A>) : 'A = getter (unbox atom) |> unbox<'A>

    let inline set<'A> (setter: Setter<obj>) (atom: AtomConfig<'A>) (value: 'A) = setter (unbox atom) value

    let inline change<'A> (setter: Setter<obj>) (atom: AtomConfig<'A>) (value: 'A -> 'A) =
        setter (unbox atom) (unbox value)


    //    type Subscription<'A> = (('A -> unit) -> JS.Promise<unit>) -> unit -> unit

    let inline addSubscription<'A> (debounce: bool) mount unmount (atom: AtomConfig<'A>) =
        let mutable mounted = false

        let getDebugInfo () = $"atom={atom} mounted={mounted}"

        Logger.logTrace (fun () -> $"{nameof FsStore} | Atom.wrap [ constructor ] {getDebugInfo ()}")

        let inline internalMount (setAtom: 'A -> unit) =
            promise {
                mounted <- true

                Logger.logTrace (fun () -> $"{nameof FsStore} | Atom.wrap [ debouncedInternalMount ] {getDebugInfo ()}")

                do! mount setAtom
            }

        let internalMount =
            if not debounce then
                internalMount >> Promise.start
            else
                Js.debounce (internalMount >> Promise.start) 0

        let inline internalUnmount () =
            if mounted then
                Logger.logTrace (fun () -> $"{nameof FsStore} | Atom.wrap [ internalUnmount ] {getDebugInfo ()}")

                unmount ()

            mounted <- false

        let inline onMount (setAtom: _ -> unit) =
            internalMount setAtom
            fun _ -> internalUnmount ()

        let newOnMount =
            match jsTypeof atom?onMount with
            | "function" ->
                let oldOnMount = atom?onMount

                fun setAtom ->
                    let atomUnsubscribe = oldOnMount setAtom
                    let newUnsubscribe = onMount setAtom

                    fun () ->
                        newUnsubscribe ()
                        atomUnsubscribe ()

            | _ -> onMount

        atom?onMount <- newOnMount

        atom

    //    let wrapWithStore<'A> ((mount, unmount): Subscription<'A>) (atom: AtomConfig<'A>) =
//        atom
//        |> wrap (fun setAtom -> promise { () }, unmount)

    let register<'A> storeAtomPath (atom: AtomConfig<'A>) =
        let getDebugInfo () =
            $"atom={atom} storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath} "

        Profiling.addCount (fun () -> $"{nameof FsStore} | Atom [ register ] {getDebugInfo ()}")

        let internalKey = AtomInternalKey (atom.ToString ())

        atomPathMap.[storeAtomPath] <- atom |> unbox<AtomConfig<obj>>
        atomIdMap.[internalKey] <- storeAtomPath

        atom

    let isRegistered atomReference =
        match atomReference with
        | AtomReference.Atom atom -> atomIdMap.ContainsKey (AtomInternalKey (atom.ToString ()))
        | AtomReference.Path path -> atomPathMap.ContainsKey path


    let rec query atomReference =
        let result =
            match atomReference with
            | AtomReference.Atom atom ->
                let internalKey = AtomInternalKey (atom.ToString ())

                match atomIdMap.TryGetValue internalKey with
                | true, value -> Some value
                | _ -> None
            | AtomReference.Path path ->
                match atomPathMap.TryGetValue path with
                | true, atom -> Some (query (AtomReference.Atom (atom |> unbox<AtomConfig<'A>>)))
                | _ -> None

        Logger.logTrace (fun () -> $"Atom.query atomReference={atomReference} result={result}")

        match result with
        | Some result -> result
        | None -> failwith $"Atom.query error atomReference={atomReference} "


    module Primitives =
        let inline atom value = jotai.atom value

        let inline selector<'A> (read: Read<'A>) (write: Write<'A>) =
            let rec atom =
                jotai.atom (
                    (fun getter ->
                        Logger.logTrace (fun () -> $"{nameof FsStore} | Atom.Primitives.selector get()")
                        read getter),
                    Some
                        (fun getter setter value ->
                            //                        Logger.logTrace (fun () -> $"{nameof FsStore} | Atom.Primitives.selector set()")
                            let newValue =
                                match jsTypeof value with
                                | "function" ->
                                    let oldValue = get getter atom
                                    (unbox value) oldValue |> unbox
                                | _ -> value

                            write getter setter newValue)
                )

            atom

        let inline readSelector (read: Read<'A>) =
            selector read (fun _ _ _ -> failwith "Atom.Primitives.readSelector is read only. (5)")

        let inline setSelector (write: Write<'A>) = selector (fun _ -> JS.undefined) write

        let inline atomFamily (defaultValueFn: 'TKey -> AtomConfig<'A>) =
            jotaiUtils.atomFamily
                (fun key ->
                    Profiling.addCount (fun () -> $"{nameof FsStore} | Atom.Primitives.atomFamily key={key}")
                    defaultValueFn key)
                (if false then JS.undefined else Object.compare)

        let inline selectAtom atom selector =
            jotaiUtils.selectAtom
                atom
                (fun getter ->
                    Profiling.addCount (fun () -> $"{nameof FsStore} | Atom.Primitives.selectAtom atom={atom}")
                    selector getter)
                (if true then JS.undefined else Object.compare)

        let inline selectorFamily<'TKey, 'A> (read: 'TKey -> Read<'A>) (write: 'TKey -> Write<'A>) =
            atomFamily (fun param -> selector (read param) (write param))

        let inline readSelectorFamily<'TKey, 'A> (read: 'TKey -> Read<'A>) : ('TKey -> AtomConfig<'A>) =
            selectorFamily read (fun _ _ _ -> failwith "Atom.Primitives.readSelectorFamily is read only.")

        let inline asyncSelector<'A> (read: AsyncRead<'A>) (write: AsyncWrite<'A>) =
            jotai.atom (
                (fun getter -> promise { return! read getter }),
                Some (fun getter setter newValue -> promise { do! write getter setter newValue })
            )

    let inline map<'A, 'B> readFn writeFn atom =
        Primitives.selector
            (fun getter ->
                let value = get getter atom
                let newValue: 'B = readFn value
                newValue)
            (fun _getter setter newValue ->
                let newValue: 'A = writeFn newValue
                set setter atom newValue)

    let inline atomFamilyAtom defaultValueFn =
        Primitives.atomFamily (fun param -> Primitives.atom (defaultValueFn param))

    let inline create atomType =
        match atomType with
        | AtomType.Atom value -> Primitives.atom value
        | AtomType.ReadSelector read -> Primitives.readSelector read
        | AtomType.Selector (read, write) -> Primitives.selector read write
        | AtomType.WriteOnlyAtom write -> Primitives.setSelector write

    let inline createRegistered storeAtomPath atomType =
        atomType |> create |> register storeAtomPath

    let inline createRegisteredWithStorage<'A> storeAtomPath (defaultValue: 'A) =
        let defaultValueFormatted = defaultValue |> Enum.formatIfEnum

        let internalAtom =
            jotaiUtils.atomWithStorage
                (storeAtomPath
                 |> StoreAtomPath.AtomPath
                 |> AtomPath.Value)
                defaultValueFormatted

        let wrapper =
            AtomType.Selector (
                (fun getter -> get getter internalAtom),
                (fun _ setter argFn ->
                    let newValue =
                        argFn
                        |> Object.invokeOrReturn
                        |> Enum.formatIfEnum

                    set setter internalAtom newValue)
            )
            |> create

        wrapper?init <- defaultValueFormatted

        wrapper |> register storeAtomPath


    let emptyArrayAtom = Primitives.atom ([||]: obj [])

    let inline waitForAll<'T> (atoms: AtomConfig<'T> []) =
        match atoms with
        | [||] -> unbox emptyArrayAtom
        | _ -> jotaiUtils.waitForAll atoms

    let inline split atom = jotaiUtils.splitAtom atom

    module Atom =
        let mutable private keyCount = 0

        let create atomType =
            keyCount <- keyCount + 1
            let key = $"atom{keyCount}"

            let rec config =
                {
                    ToString = fun () -> key
                    init =
                        match atomType with
                        | AtomType.Atom value -> Some value
                        | _ -> None
                    Read =
                        match atomType with
                        | AtomType.Atom _ -> fun getter -> get getter config
                        | AtomType.ReadSelector read -> read
                        | AtomType.Selector (read, _) -> read
                        | AtomType.WriteOnlyAtom _ -> JS.undefined
                    Write =
                        match atomType with
                        | AtomType.Atom _ -> fun _ setter -> set setter config
                        | AtomType.ReadSelector _ -> JS.undefined
                        | AtomType.Selector (_, write) -> write
                        | AtomType.WriteOnlyAtom write -> write
                }

            config

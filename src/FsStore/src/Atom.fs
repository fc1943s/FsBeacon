namespace FsStore

open System.Collections.Generic
open Fable.Core
open FsJs.Dom
open FsStore.Bindings.Jotai
open FsStore.Model
open Microsoft.FSharp.Core.Operators
open FsJs
open Fable.Core.JsInterop

#nowarn "40"


module Atom =
    type AtomInternalKey = AtomInternalKey of key: string
    let private atomPathMap<'T> = Dictionary<StoreAtomPath, AtomConfig<'T>> ()
    let private atomIdMap<'T> = Dictionary<AtomInternalKey, StoreAtomPath> ()

    Global.set (nameof atomPathMap) atomPathMap
    Global.set (nameof atomIdMap) atomIdMap


    let inline get<'A> (getter: Getter<obj>) (atom: AtomConfig<'A>) : 'A = getter (unbox atom) |> unbox<'A>

    let inline set<'A> (setter: Setter<obj>) (atom: AtomConfig<'A>) (value: 'A) = setter (unbox atom) value

    let inline change<'A> (setter: Setter<obj>) (atom: AtomConfig<'A>) (value: 'A -> 'A) =
        setter (unbox atom) (unbox value)

    let register storeAtomPath atom =
        Profiling.addCount $"# Atom.register storeAtomPath={storeAtomPath} atom={atom}"
        let internalKey = AtomInternalKey (atom.ToString ())
        atomPathMap.[storeAtomPath] <- atom
        atomIdMap.[internalKey] <- storeAtomPath
        atom

    module Primitives =
        let inline atom value = jotai.atom value

        let inline selector (read: Read<'A>) (write: Write<'A>) =
            let rec atom =
                jotai.atom ( //(read, Some write)
                    (fun getter ->
                        Profiling.addCount $"# Atom.Primitives.rawSelector get() atom={atom}"
                        read getter),
                    Some
                        (fun getter setter value ->
                            Profiling.addCount $"# Atom.Primitives.rawSelector set() atom={atom}"
                            let newValue = value
                            //                        match jsTypeof value with
                            //                         | "function" -> (unbox value) () |> unbox
                            //                         | _ -> value
                            write getter setter newValue)
                )

            atom

        let inline readSelector (read: Read<'A>) =
            selector read (fun _ _ _ -> failwith "Atom.Primitives.readSelector is read only.")

        let inline setSelector (write: Write<'A>) = selector (unbox null) write

        let inline atomFamily (defaultValueFn: 'TKey -> AtomConfig<'A>) =
            jotaiUtils.atomFamily defaultValueFn (if true then JS.undefined else Object.compare)

        let inline selectAtom atom selector =
            jotaiUtils.selectAtom atom selector (if true then JS.undefined else Object.compare)

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

    let inline createRegisteredWithStorage storeAtomPath defaultValue =
        let defaultValueFormatted = defaultValue |> Enum.formatIfEnum

        let internalAtom =
            jotaiUtils.atomWithStorage
                (storeAtomPath
                 |> StoreAtomPath.AtomPath
                 |> AtomPath.Value)
                defaultValueFormatted

        let selectorWrapper =
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

        selectorWrapper?init <- defaultValueFormatted

        selectorWrapper |> register storeAtomPath

    let rec query atomReference =
        let result =
            match atomReference with
            | AtomReference.Atom atom ->
                let internalKey = AtomInternalKey (atom.ToString ())

                match atomIdMap.TryGetValue internalKey with
                | true, value -> value
                | _ -> failwith $"Internal.queryAtomPath query error atomReference={atomReference} "
            | AtomReference.Path path ->
                match atomPathMap.TryGetValue path with
                | true, atom -> query (AtomReference.Atom atom)
                | _ -> failwith $"Internal.queryAtomPath query error atomReference={atomReference} "

        Logger.logTrace (fun () -> $"Internal.queryAtomPath atomReference={atomReference} result={result}")

        result


    let mutable private keyCount = 0

    let create2 atomType =
        keyCount <- keyCount + 1
        let key = $"atom{keyCount}"

        let rec config =
            {
                ToString = fun () -> key
                DefaultValue =
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
                Trigger = JS.undefined
            }

        config

    let inline addTrigger (atom: AtomConfig<_>) =
        { atom with
            Trigger = create (AtomType.Atom 0)
        }

    let inline enableAdapters (atom: AtomConfig<_>) =
        eprintf "enableAdapters called"
        atom

    let emptyArrayAtom = Primitives.atom ([||]: obj [])

    let inline waitForAll<'T> (atoms: AtomConfig<'T> []) =
        match atoms with
        | [||] -> unbox emptyArrayAtom
        | _ -> jotaiUtils.waitForAll atoms

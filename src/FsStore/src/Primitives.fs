namespace FsStore

open Fable.Core.JsInterop
open FsStore.Model
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs
open FsStore.Bindings.Jotai

#nowarn "40"


module Internal =

    //    [<RequireQualifiedAccess>]
//    type AtomType =
//        | Atom
//        | AtomWithSync
//        | AtomWithStorage
//        | AtomWithStorageSync

    //    let queryAtomPath atomReference =
//        let result =
//            match atomReference with
//            | AtomReference.Atom atom ->
//                match Atom.atomIdMap.TryGetValue (atom.ToString ()) with
//                | true, value -> AtomPath value
//                | _ -> failwith $"Internal.queryAtomPath query error atomReference={atomReference} "
//            | AtomReference.Path path ->
//                match atomPathMap.TryGetValue path with
//                | true, value -> AtomPath value
//                | _ -> failwith $"Internal.queryAtomPath query error atomReference={atomReference} "
//
//        Logger.logTrace (fun () -> $"Internal.queryAtomPath atomReference={atomReference} result={result}")
//
//        result
    ()


module OldPrimitives =
    //    let inline atomRegistered<'A> storeAtomPath (defaultValue: 'A) =
//        jotai.atom defaultValue
//        |> Atom.register storeAtomPath


    //    let inline selector<'A> storeAtomPath (read: Read<'A>) (write: Write<'A>) =
//        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath
//
//        Atom.Primitives.rawSelector
//            (fun getter ->
//                Profiling.addCount (fun () -> $"# Primitives.selector get {atomPath}"
//                read getter)
//            (fun getter setter value ->
//                Profiling.addCount (fun () -> $"# Primitives.selector set {atomPath}"
//                let newValue = value
//                //                        match jsTypeof value with
//                //                         | "function" -> (unbox value) () |> unbox
//                //                         | _ -> value
//                write getter setter newValue)


    //    let inline selectAtom atom selector =
//        jotaiUtils.selectAtom atom selector JS.undefined

    ()


[<AutoOpen>]
module PrimitivesMagic =
    module OldStore =
        let inline atom<'A> storeRoot name (defaultValue: 'A) =
            Atom.createRegistered (RootAtomPath (storeRoot, AtomName name)) (AtomType.Atom defaultValue)


        let inline atomFamilyRegistered<'TKey, 'A>
            storeRoot
            collection
            name
            (defaultValueFn: 'TKey -> 'A)
            keysFormatter
            =
            Atom.Primitives.atomFamily
                (fun param ->
                    Atom.createRegistered
                        (IndexedAtomPath (storeRoot, collection, keysFormatter param, AtomName name))
                        (AtomType.Atom (defaultValueFn param)))


        let inline selector<'A> (read: Read<'A>) (write: Write<'A>) = Atom.Primitives.selector read write

        let inline readSelector<'A> (read: Read<'A>) =
            selector<'A> read (fun _ _ _ -> failwith $"Primitives.readSelector is read only.")



        let inline writeOnlyAtom internalAtom =
            Atom.Primitives.setSelector
                (fun _getter setter newValue ->
                    Logger.logTrace (fun () -> $"writeOnlyAtom internalAtom={internalAtom} newValue={newValue}")
                    Atom.set setter internalAtom newValue)

        let inline selectAtomFamily atom selector =
            Atom.Primitives.atomFamily (fun param -> Atom.Primitives.selectAtom atom (selector param))


        //        let inline atomWithStorage2 (atomPath: RootAtomPath) defaultValue =
//            let atomPath =
//                {
//                    StoreRoot = storeRoot
//                    Collection = None
//                    Keys = []
//                    Name = Some (AtomName name)
//                }
//                |> AtomKey.AtomPath
//
//
//            let defaultValueFormatted = defaultValue |> Enum.formatIfEnum
//
//            let internalAtom = jotaiUtils.atomWithStorage (atomPath |> AtomPath.Value) defaultValueFormatted
//
//            let selectorWrapper =
//                selector
//                    storeRoot
//                    name
//                    (fun getter -> value getter internalAtom)
//                    (fun _ setter argFn ->
//                        let newValue = argFn |> Object.invokeOrReturn |> Enum.formatIfEnum
//                        set setter internalAtom newValue)
//
//            selectorWrapper?init <- defaultValueFormatted
//
//            Internal.registerAtom Internal.AtomType.AtomWithStorage atomPath selectorWrapper
//
//            selectorWrapper

        let inline atomWithStorage storeAtomPath defaultValue =
            let defaultValueFormatted = defaultValue |> Enum.formatIfEnum

            let internalAtom =
                jotaiUtils.atomWithStorage
                    (storeAtomPath
                     |> StoreAtomPath.AtomPath
                     |> AtomPath.Value)
                    defaultValueFormatted

            let selectorWrapper =
                selector
                    (fun getter -> Atom.get getter internalAtom)
                    (fun _ setter argFn ->
                        let newValue =
                            argFn
                            |> Object.invokeOrReturn
                            |> Enum.formatIfEnum

                        Atom.set setter internalAtom newValue)

            selectorWrapper?init <- defaultValueFormatted

            selectorWrapper


        let inline asyncReadSelector<'A> (read: AsyncRead<'A>) =
            Atom.Primitives.asyncSelector
                read
                (fun _ _ _newValue -> promise { failwith "Primitives.asyncReadSelector is read only." })


        let inline asyncSelectorFamily<'TKey, 'A> (read: 'TKey -> AsyncRead<'A>) (write: 'TKey -> AsyncWrite<'A>) =
            Atom.Primitives.atomFamily
                (fun param ->
                    Atom.Primitives.asyncSelector
                        (read param)
                        (fun getter setter newValue -> promise { do! write param getter setter newValue }))

        let inline asyncReadSelectorFamily<'TKey, 'A> (read: 'TKey -> AsyncRead<'A>) =
            asyncSelectorFamily
                read
                (fun _key _ _ _newValue -> promise { failwith $"Primitives.asyncReadSelectorFamily is read only." })

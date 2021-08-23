namespace FsStore

open FsStore.Model
open FsCore
open FsStore.State.Atoms
open Microsoft.FSharp.Core.Operators
open FsJs
open FsStore.Bindings.Jotai


[<AutoOpen>]
module TempAtomStore =
    module Store =
        let emptyAtom = Atom.Primitives.atom ()

        let inline getAtomField (atom: InputAtom<'TValue> option) (inputScope: AtomScope) =
            match atom with
            | Some (InputAtom atomReference) ->
                let current =
                    match atomReference with
                    | AtomReference.Atom atom -> Some atom
                    | _ -> Some (unbox emptyAtom)

                let temp =
                    //                    Dom.log
                    //                        (fun () -> $"getAtomField atomPath={atomPath} queryAtomPath atomPath={queryAtomPath atomPath}")

                    match Atom.query atomReference, inputScope with
                    | storeAtomPath, AtomScope.Temp -> Some (Join.tempValue storeAtomPath)
                    | _ -> None

                current, temp
            | _ -> None, None

        let inline setTempValue<'TValue9, 'TKey> (setter: Setter<obj>) (atom: AtomConfig<'TValue9>) (value: 'TValue9) =
            let _, tempAtom = getAtomField (Some (InputAtom (AtomReference.Atom atom))) AtomScope.Temp

            match tempAtom with
            | Some atom ->
                let newValueJson =
                    match Json.encode<'TValue9> value with
                    | String.Valid json -> Some json
                    | _ -> None

                Atom.set setter atom newValueJson
            | _ -> ()

        let inline scopedSet<'TValue10, 'TKey>
            (setter: Setter<obj>)
            (atomScope: AtomScope)
            (atom: 'TKey -> AtomConfig<'TValue10>, key: 'TKey, value: 'TValue10)
            =
            match atomScope with
            | AtomScope.Current -> Atom.set setter (atom key) value
            | AtomScope.Temp -> setTempValue<'TValue10, 'TKey> setter (atom key) value

        let inline resetTempValue<'TValue8, 'TKey> (setter: Setter<obj>) (atom: AtomConfig<'TValue8>) =
            let _, tempAtom = getAtomField (Some (InputAtom (AtomReference.Atom atom))) AtomScope.Temp

            match tempAtom with
            | Some atom -> Atom.set setter atom None
            | _ -> ()

        let rec ___emptyTempAtom = nameof ___emptyTempAtom

        let inline getTempValue<'TValue11, 'TKey> getter (atom: AtomConfig<'TValue11>) =
            let _, tempAtom = getAtomField (Some (InputAtom (AtomReference.Atom atom))) AtomScope.Temp

            match tempAtom with
            | Some tempAtom ->
                let result = Atom.get getter tempAtom

                match result with
                | Some result when result = ___emptyTempAtom -> unbox null
                | None -> Atom.get getter atom
                | Some result -> Json.decode<'TValue11> result
            | _ -> Atom.get getter atom

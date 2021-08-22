namespace FsStore

open FsStore.Model
open FsStore.State.Atoms
open Microsoft.FSharp.Core.Operators
open FsJs
open FsStore.Bindings.Jotai


[<AutoOpen>]
module TempAtomStore =
    module Store =
        let emptyAtom = jotai.atom<obj> null

        let inline getAtomField (atom: InputAtom<'TValue> option) (inputScope: AtomScope) =
            match atom with
            | Some (InputAtom atomPath) ->
                let current =
                    match atomPath with
                    | AtomReference.Atom atom -> Some atom
                    | _ -> Some (unbox emptyAtom)

                let temp =
                    //                    Dom.log
                    //                        (fun () -> $"getAtomField atomPath={atomPath} queryAtomPath atomPath={queryAtomPath atomPath}")

                    match Internal.queryAtomPath atomPath, inputScope with
                    | atomPath, AtomScope.Temp -> Some (Join.tempValue atomPath)
                    | _ -> None

                current, temp
            | _ -> None, None

        let inline setTempValue<'TValue9, 'TKey> (setter: SetFn) (atom: Atom<'TValue9>) (value: 'TValue9) =
            let _, tempAtom = getAtomField (Some (InputAtom (AtomReference.Atom atom))) AtomScope.Temp

            match tempAtom with
            | Some atom -> Store.set setter atom (value |> Json.encode<'TValue9>)
            | _ -> ()

        let inline scopedSet<'TValue10, 'TKey>
            (setter: SetFn)
            (atomScope: AtomScope)
            (atom: 'TKey -> Atom<'TValue10>, key: 'TKey, value: 'TValue10)
            =
            match atomScope with
            | AtomScope.Current -> Store.set setter (atom key) value
            | AtomScope.Temp -> setTempValue<'TValue10, 'TKey> setter (atom key) value

        let inline resetTempValue<'TValue8, 'TKey> (setter: SetFn) (atom: Atom<'TValue8>) =
            let _, tempAtom = getAtomField (Some (InputAtom (AtomReference.Atom atom))) AtomScope.Temp

            match tempAtom with
            | Some atom -> Store.set setter atom null
            | _ -> ()

        let rec ___emptyTempAtom = nameof ___emptyTempAtom

        let inline getTempValue<'TValue11, 'TKey> getter (atom: Atom<'TValue11>) =
            let _, tempAtom = getAtomField (Some (InputAtom (AtomReference.Atom atom))) AtomScope.Temp

            match tempAtom with
            | Some tempAtom ->
                let result = Store.value getter tempAtom

                match result with
                | result when result = ___emptyTempAtom -> unbox null
                | null -> Store.value getter atom
                | _ -> Json.decode<'TValue11> result
            | _ -> Store.value getter atom

namespace FsStore.Store

open Fable.Core.JsInterop
open FsStore
open FsStore.State
open Microsoft.FSharp.Core.Operators
open FsCore


[<AutoOpen>]
module AtomWithStorageSync =
    module Store =
        let inline atomWithStorageSync<'TKey, 'TValue when 'TValue: equality> storeAtomPath defaultValue =
            let storageAtom = Atom.createRegisteredWithStorage storeAtomPath defaultValue
            let syncAtom = Store.atomWithSync<'TKey, 'TValue> storeAtomPath defaultValue

            let mutable lastSetAtom: ('TValue option -> unit) option = None
            let mutable lastValue = None

            let rec wrapper =
                Atom.Primitives.selector
                    (fun getter ->
                        match Atom.get getter syncAtom, Atom.get getter storageAtom with
                        | syncValue, storageValue when
                            syncValue |> Object.compare defaultValue
                            && (storageValue |> Object.compare defaultValue
                                || (Atom.get getter Selectors.Gun.alias).IsNone
                                || lastValue.IsNone)
                            ->
                            Atom.get getter storageAtom
                        | syncValue, _ ->
                            match lastSetAtom with
                            | Some lastSetAtom when
                                lastValue.IsNone
                                || lastValue
                                   |> Object.compare (Some syncValue)
                                   |> not
                                ->
                                lastValue <- Some syncValue
                                lastSetAtom (Some syncValue)
                            | _ -> ()

                            syncValue)
                    (fun _get setter newValue ->
                        if lastValue.IsNone
                           || lastValue |> Object.compare (Some newValue) |> not then
                            lastValue <- Some newValue
                            Atom.set setter syncAtom newValue

                        Atom.set setter storageAtom newValue)

            //            wrapper?onMount <- fun (setAtom: 'TValue option -> unit) ->
//                                   lastSetAtom <- Some setAtom
//                                   fun () -> lastSetAtom <- None

            wrapper?init <- defaultValue

            wrapper

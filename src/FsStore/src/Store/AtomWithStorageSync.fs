namespace FsStore.Store

open Fable.Core.JsInterop
open FsStore
open FsStore.Model
open Microsoft.FSharp.Core.Operators
open FsCore


[<AutoOpen>]
module AtomWithStorageSync =
    module Store =
        let inline atomWithStorageSync<'TKey, 'TValue> storeRoot name defaultValue =
            let atomKey =
                {
                    StoreRoot = storeRoot
                    Collection = None
                    Keys = []
                    Name = name
                }

            let storageAtom = Store.atomWithStorage storeRoot name defaultValue

            let syncAtom = Store.atomWithSync<'TKey, 'TValue> atomKey defaultValue

            let mutable lastSetAtom: ('TValue option -> unit) option = None
            let mutable lastValue = None

            let rec wrapper =
                Store.selector
                    storeRoot
                    name
                    (fun getter ->
                        match Store.value getter syncAtom, Store.value getter storageAtom with
                        | syncValue, storageValue when
                            syncValue |> Object.compare defaultValue
                            && (storageValue |> Object.compare defaultValue
                                || (Store.value getter Selectors.Gun.alias).IsNone
                                || lastValue.IsNone)
                            ->
                            Store.value getter storageAtom
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
                            Store.set setter syncAtom newValue

                        Store.set setter storageAtom newValue)

            wrapper?onMount <- fun (setAtom: 'TValue option -> unit) ->
                                   lastSetAtom <- Some setAtom
                                   fun () -> lastSetAtom <- None

            wrapper?init <- defaultValue

            Internal.registerAtom Internal.AtomType.AtomWithStorageSync (atomKey |> AtomKey.AtomPath) wrapper

            wrapper

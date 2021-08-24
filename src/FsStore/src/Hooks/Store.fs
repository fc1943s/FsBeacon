namespace FsStore.Hooks

open System.Collections.Generic
open Fable.Core
open FsJs
open FsStore
open FsStore.Model
open FsStore.State
open FsStore.Store
open Microsoft.FSharp.Core.Operators
open Feliz
open FsCore
open FsStore.Bindings
open FsStore.Bindings.Jotai


module Store =
    let inline useAtom atom = jotai.useAtom atom // AtomScope.Current

    let inline useStateOption (atom: AtomConfig<'TValue5> option) =
        let flatAtom =
            React.useMemo (
                (fun () ->
                    match atom with
                    | Some atom -> atom
                    | None -> Store.emptyAtom |> unbox<AtomConfig<'TValue5>>),
                [|
                    box atom
                |]
            )

        let value, setValue = useAtom flatAtom

        (if atom.IsNone then None else Some value), (if atom.IsNone then (fun _ -> ()) else setValue)

    let inline useValue atom = jotaiUtils.useAtomValue atom

    let inline useValueTuple a b =
        let a = useValue a
        let b = useValue b
        a, b

    let inline useScopeState<'TValue7> (atom: InputAtom<'TValue7> option) (inputScope: InputScope<'TValue7> option) =
        let logger = useValue Selectors.logger

        let currentAtomField, tempAtomField =
            React.useMemo (
                (fun () -> Store.getAtomField atom (InputScope<_>.AtomScope inputScope)),
                [|
                    box atom
                    box inputScope
                |]
            )

        let currentValue, setCurrentValue = useStateOption currentAtomField
        let tempValue, setTempValue = useStateOption tempAtomField

        React.useMemo (
            (fun () ->
                let defaultJsonEncode, _defaultJsonDecode = unbox Gun.defaultSerializer

                let newTempValue =
                    match inputScope, tempValue |> Option.defaultValue None with
                    | _, tempValue when tempValue = Some Store.___emptyTempAtom -> unbox null
                    | _, None -> currentValue |> Option.defaultValue (unbox null)
                    | Some (InputScope.Temp (_, jsonDecode)), Some tempValue ->
                        try
                            logger.Debug
                                (fun () ->
                                    $"useTempAtom
                                    currentValue={currentValue}
                                    atom={atom}
                                    tempValue={tempValue}")

                            jsonDecode tempValue
                        with
                        | ex ->
                            printfn $"Error decoding tempValue={tempValue} ex={ex}"

                            currentValue
                            |> Option.defaultValue (unbox tempValue)
                    | _ ->
                        currentValue
                        |> Option.defaultValue (unbox tempValue)

                let setTempValue =
                    if atom.IsSome then
                        (fun newValue ->
                            setTempValue (
                                match box newValue with
                                | null -> Some Store.___emptyTempAtom
                                | _ ->
                                    match inputScope with
                                    | Some (InputScope.Temp (jsonEncode, _)) -> Some (jsonEncode newValue)
                                    | _ -> defaultJsonEncode newValue
                            ))
                    else
                        (fun _ -> printfn "empty set #1")

                let setCurrentValue =
                    if atom.IsSome then
                        setCurrentValue
                    else
                        (fun _ -> printfn "empty set #2")

                {|
                    Value =
                        match inputScope with
                        | Some (InputScope.Temp _) -> newTempValue
                        | _ -> currentValue |> Option.defaultValue (unbox null)
                    SetValue =
                        match inputScope with
                        | Some (InputScope.Temp _) -> setTempValue
                        | _ -> setCurrentValue
                    CurrentValue = currentValue |> Option.defaultValue (unbox null)
                    SetCurrentValue = setCurrentValue
                    TempValue = newTempValue
                    SetTempValue = setTempValue
                |}),
            [|
                box logger
                box inputScope
                box atom
                box currentValue
                box tempValue
                box setCurrentValue
                box setTempValue
            |]
        )

    let inline useTempState<'T> (atomReference: AtomReference<'T>) =
        let inputAtom, inputScope =
            React.useMemo (
                (fun () -> Some (InputAtom atomReference), Some (InputScope.Temp Gun.defaultSerializer)),
                [|
                    box atomReference
                |]
            )

        useScopeState<'T> inputAtom inputScope

    let inline useAtomTempState<'T> (atom: AtomConfig<'T>) =
        let atomReference =
            React.useMemo (
                (fun () -> AtomReference.Atom atom),
                [|
                    box atom
                |]
            )

        useTempState<'T> atomReference

    let inline useCallbackRef (fn: Getter<obj> -> Setter<obj> -> 'a -> JS.Promise<'c>) : ('a -> JS.Promise<'c>) =
        let fnCallback = React.useCallbackRef (fun (getter, setter, arg) -> fn getter setter arg)

        let atom =
            React.useMemo (
                (fun () ->
                    Atom.Primitives.setSelector
                        (fun getter setter (arg, resolve, err) ->
                            try
                                resolve (fnCallback (getter, setter, arg))
                            with
                            | ex ->
                                printfn $"atomCallback fn error: {ex}"
                                err ex

                            ())),
                [|
                    box fnCallback
                |]
            )

        let _value, setValue = useAtom atom

        React.useCallback (
            (fun arg ->
                Promise.create (fun resolve err -> setValue (arg, resolve, err))
                |> Promise.bind id),
            [|
                box setValue
            |]
        )

    let inline useStore () =
        useCallbackRef (fun getter setter () -> promise { return (getter, setter) })

    let inline useState atom = useAtom atom

    let inline useSetState atom = jotaiUtils.useUpdateAtom atom

    //    let inline useSetStatePrev<'T> atom =
//        let setter = jotaiUtils.useUpdateAtom<'T> atom
//        fun (value: 'T -> 'T) -> setter (unbox value)

    let rec globalHashCache = Dom.Global.register (nameof globalHashCache) (Dictionary<obj, bool> ())

    let inline useHashedEffectOnce hash fn =
        let store = useStore ()

        React.useEffectOnce
            (fun () ->
                let hashCache = globalHashCache.Get ()

                if not (hashCache.ContainsKey hash) then
                    hashCache.[hash] <- true

                    promise {
                        let! getter, setter = store ()
                        do! fn getter setter
                    }
                    |> Promise.start)

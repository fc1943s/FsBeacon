namespace FsStore.Bindings

open System
open Fable.Core.JsInterop
open Fable.Core
open FsCore
open Microsoft.FSharp.Core.Operators
open FsJs


module Batcher =
    let interval = 15

    type Cb<'TFnResult> = unit -> 'TFnResult

    let private internalBatcher
        (_fn: 'TKey [] -> Cb<'TFnResult> -> unit)
        (_settings: {| interval: int |})
        : 'TKey -> Cb<'TFnResult> -> unit =
        importDefault "batcher-js"

    //    let batcher fn settings =
//        let newFn = internalBatcher (fun (x: 'TKey []) _lock -> fn x) settings
//        let lock = fun () -> ()
//
//        fun (x: 'TKey) ->
//            Js.jsCall newFn x lock
//            ()
    //
//    let batcher2<'TKey,'TFnResult> =
//        batcher
//        |> unbox<('TKey [] -> unit) -> {| interval: int |} -> ('TKey -> unit)>
//        : ('TKey [] -> unit) -> {| interval: int |} -> ('TKey -> unit) = unbox batcher

    [<RequireQualifiedAccess>]
    type DataType =
        | Key
        | Data

    [<RequireQualifiedAccess>]
    type BatchType<'TKey, 'TValue> =
        | KeysFromServer of
            atomType: (DataType * Type) *
            keys: 'TKey [] *
            ticks: TicksGuid *
            trigger: ((TicksGuid * 'TKey []) [] -> JS.Promise<unit>)
        | Data of
            ticks: TicksGuid *
            data: 'TValue *
            key: string *
            trigger: (TicksGuid * 'TValue * string -> JS.Promise<unit>)
        | Subscribe of ticks: TicksGuid * trigger: (TicksGuid -> JS.Promise<IDisposable>)
        | Set of ticks: TicksGuid * trigger: (TicksGuid -> JS.Promise<unit>)

    let inline macroQueue fn =
        JS.setTimeout (fn >> Promise.start) 0 |> ignore
    //        fn () |> Promise.start

    let inline macroQueue2 fn = JS.setTimeout fn 0 |> ignore

    let inline wrapTry fn =
        try
            fn ()
        with
        | ex ->
            Logger.logError (fun () -> $"wrapTry error: {ex.Message}")
            Logger.consoleError [| ex |]
            JS.undefined

    let inline internalBatch (itemsArray: BatchType<obj, obj> []) =
        promise {
            let items =
                itemsArray
                |> Array.map
                    (function
                    | BatchType.Set (ticks, trigger) -> Some (ticks, trigger), None, None, None
                    | BatchType.Subscribe (ticks, trigger) -> None, Some (ticks, trigger), None, None
                    | BatchType.Data (ticks, data, key, trigger) -> None, None, Some (ticks, data, key, trigger), None
                    | BatchType.KeysFromServer (atomType, item, ticks, trigger) ->
                        None, None, None, Some (atomType, item, ticks, trigger))

            let! setDataDisposables =
                items
                |> Array.choose (fun (setFn, _, _, _) -> setFn)
                |> Array.map (fun (ticks, setFn) -> wrapTry (fun () -> setFn ticks))
                |> Promise.all

            let! subscribeDisposables =
                items
                |> Array.choose (fun (_, subscribeFn, _, _) -> subscribeFn)
                |> Array.map (fun (ticks, subscribeFn) -> wrapTry (fun () -> subscribeFn ticks))
                |> Promise.all

            let! providerDisposables =
                let providerData =
                    items
                    |> Array.choose (fun (_, _, data, _) -> data)

                match providerData with
                | [||] -> [||]
                | _ ->
                    let providerData =
                        providerData
                        |> Array.map (fun (ticks, data, key, trigger) -> fun () -> trigger (ticks, data, key))

                    providerData |> Array.map wrapTry
                |> Promise.all

            let! keysDisposables =
                let keysFromServer =
                    items
                    |> Array.choose (fun (_, _, _, keys) -> keys)

                match keysFromServer with
                | [||] -> [||]
                | _ ->
                    keysFromServer
                    |> Array.groupBy (fun (atomType, _, _, _) -> atomType)
                    |> Array.map
                        (fun (_, group) ->
                            let trigger =
                                group
                                |> Array.last
                                |> fun (_, _, _, trigger) -> trigger

                            group
                            |> Array.map (fun (_, item, ticks, _) -> ticks, item)
                            |> trigger)
                |> Promise.all

            Profiling.addTimestamp
                (fun () ->
                    $"($$) ---- #3b setDataDisposables={Json.encodeWithNull setDataDisposables} subscribeDisposables={Json.encodeWithNull subscribeDisposables} providerDisposables={Json.encodeWithNull providerDisposables} keysDisposables={Json.encodeWithNull keysDisposables} ")
        }

    let (newFn: BatchType<obj, obj> -> Cb<obj> -> unit), lock =
        let settings = {| interval = interval |}
        let newFn = internalBatcher (fun (x: _ []) _lock -> x |> internalBatch |> Promise.start) settings
        let lock = fun () -> ()
        newFn, lock

    let inline batchObj (x: BatchType<obj, obj>) =
        Js.jsCall newFn x lock
        ()




    //        fun item ->

    //            match item with/--
    //            | BatchType.Set _
//            | BatchType.Subscribe _ -> /--
    //                macroQueue2 (fun () ->
//                internalBatch [| item |] /--
    //                )
//            | _ ->/--

    //        batcher internalBatch {| interval = interval |} //item

    //    let batch2<'TKey,'TFnResult> =
//        batcher
//        |> unbox<('TKey [] -> unit) -> {| interval: int |} -> ('TKey -> unit)>
//        : ('TKey [] -> unit) -> {| interval: int |} -> ('TKey -> unit) = unbox batcher

    let batch<'TKey, 'TValue> =
        batchObj
        |> unbox<BatchType<'TKey, 'TValue> -> unit>

//    let debouncedBatchObj =
//        Js.debounce
//            (fun (batchType: BatchType<obj, obj>) ->
//                Profiling.addTimestamp
//                    (fun () ->
//                        $"""($$) ---- #2 batchType={match batchType with
//                                                    | BatchType.Set (ticks, _) -> $"Set ticks={ticks}"
//                                                    | BatchType.Subscribe (ticks, _) -> $"Subscribe ticks={ticks}"
//                                                    | BatchType.Data (data, ticks, _) -> $"Data ticks={ticks} data={data}"
//                                                    | BatchType.KeysFromServer (item, ticks, _) -> $"KeysFromServer ticks={ticks} item={item}"} """)
//
//                batch batchType)
//            0
//
//    let debouncedBatch<'TKey, 'TValue> =
//        debouncedBatchObj
//        |> unbox<BatchType<'TKey, 'TValue> -> unit>

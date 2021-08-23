namespace FsStore

open FsCore
open Fable.Core.JsInterop


module Enum =
    let inline formatIfEnum<'T> (value: 'T) =
        if typeof<'T>.IsEnum then value |> Enum.name |> unbox else value


module Object =
    let inline invokeOrReturnParam param argFn =
        match jsTypeof argFn with
        | "function" -> (argFn |> box |> unbox) param |> unbox
        | _ -> argFn

    let inline invokeOrReturn argFn = invokeOrReturnParam () argFn

    let inline compare<'T> (a: 'T) (b: 'T) = (unbox a) = (unbox b)


module Profiling =
    let measureTimeN n name fn =
        Browser.Dom.console.time name

        for i in 0 .. n do
            fn ()

        Browser.Dom.console.timeEnd name

    let measureTime = measureTimeN 999999

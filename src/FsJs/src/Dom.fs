namespace FsJs

open System
open Browser.Types
open Fable.Extras
open FsCore
open System.Collections.Generic
open Fable.Core.JsInterop
open Fable.Core
open FsCore.Model


module Dom =
    let inline window () =
        if jsTypeof Browser.Dom.window <> "undefined" then
            Some Browser.Dom.window
        else
            printfn "No window found"
            None

    module Global =

        let private ``global`` = Dictionary<string, obj> ()

        match window () with
        | Some window -> window?_global <- ``global``
        | None -> ()

        let get<'T> (key: string) (defaultValue: 'T) =
            match ``global``.TryGetValue key with
            | true, value -> value |> unbox<'T>
            | _ -> defaultValue

        let set key value = ``global``.[key] <- value

    type DeviceInfo =
        {
            Brands: (string * string) []
            IsMobile: bool
            IsElectron: bool
            IsExtension: bool
            GitHubPages: bool
            IsTesting: bool
            DeviceId: DeviceId
        }
        static member inline Default =
            {
                Brands = [||]
                IsMobile = false
                IsElectron = false
                IsExtension = false
                GitHubPages = false
                IsTesting = false
                DeviceId = DeviceId Guid.Empty
            }


    let deviceInfo =
        match window () with
        | None ->
            printfn "deviceInfo: no window found"
            DeviceInfo.Default
        | Some window ->
            let userAgentData =
                window?navigator
                |> Option.ofObjUnbox
                |> Option.bind
                    (fun navigator ->
                        navigator?userAgentData
                        |> Option.ofObjUnbox<{| mobile: bool
                                                brands: {| brand: string; version: string |} [] |}>)

            let brands =
                userAgentData
                |> Option.map
                    (fun userAgentData ->
                        userAgentData.brands
                        |> Array.map (fun brand -> brand.brand, brand.version))
                |> Option.defaultValue [||]

            let userAgentDataMobile =
                userAgentData
                |> Option.map (fun userAgentData -> userAgentData.mobile)
                |> Option.defaultValue false

            let isTesting = Js.jestWorkerId || window?Cypress <> null

            let deviceId =
                match window.localStorage.getItem "deviceId" with
                | String.ValidString deviceId -> DeviceId (Guid deviceId)
                | _ ->
                    let deviceId = DeviceId.NewId ()
                    window.localStorage.setItem ("deviceId", deviceId |> DeviceId.Value |> string)
                    deviceId

            {
                Brands = brands
                IsMobile =
                    if userAgentDataMobile then
                        true
                    elif brands.Length > 0 then
                        false
                    else
                        let userAgent = if window?navigator = None then "" else window?navigator?userAgent

                        JSe
                            .RegExp(
                                "Android|BlackBerry|iPhone|iPad|iPod|Opera Mini|IEMobile|WPDesktop",
                                JSe.RegExpFlag().i
                            )
                            .Test userAgent
                IsElectron = jsTypeof window?electronApi = "object"
                IsExtension = window.location.protocol = "chrome-extension:"
                GitHubPages = window.location.host.EndsWith "github.io"
                IsTesting = isTesting
                DeviceId = deviceId
            }

    let isDebugStatic =
        not deviceInfo.GitHubPages
        && not deviceInfo.IsExtension
        && not deviceInfo.IsElectron
        && not deviceInfo.IsMobile

    Global.set "Debug" false
    if window?Cypress <> null then Global.set "Debug" true
    Global.set "Debug" true

    let inline isDebug () =
        let debug = Global.get "Debug" false
        debug <> false && (debug || isDebugStatic)



    module ConsoleFlag =
        let reset = "\x1b[0m"
        let bright = "\x1b[1m"
        let dim = "\x1b[2m"
        let underscore = "\x1b[4m"
        let blink = "\x1b[5m"
        let reverse = "\x1b[7m"
        let hidden = "\x1b[8m"

        let fgBlack = "\x1b[30m"
        let fgRed = "\x1b[31m"
        let fgGreen = "\x1b[32m"
        let fgYellow = "\x1b[33m"
        let fgBlue = "\x1b[34m"
        let fgMagenta = "\x1b[35m"
        let fgCyan = "\x1b[36m"
        let fgWhite = "\x1b[37m"

        let fg =
            [
                fgBlack
                fgRed
                fgGreen
                fgYellow
                fgBlue
                fgMagenta
                fgCyan
                fgWhite
            ]

        let bgBlack = "\x1b[40m"
        let bgRed = "\x1b[41m"
        let bgGreen = "\x1b[42m"
        let bgYellow = "\x1b[43m"
        let bgBlue = "\x1b[44m"
        let bgMagenta = "\x1b[45m"
        let bgCyan = "\x1b[46m"
        let bgWhite = "\x1b[47m"


    let deviceTag =
        deviceInfo.DeviceId
        |> DeviceId.Value
        |> string
        |> String.substringFrom -4

    let inline logWithFn logFn fn =
        let result = fn ()

        if result |> Option.ofObjUnbox |> Option.isSome then

            let output =
                [|
                    let tagValue =
                        deviceTag
                        |> Seq.map Char.getNumericValue
                        |> Seq.map Math.Abs
                        |> Seq.map float
                        |> Seq.sum

                    let tagIndex = ((tagValue / 60.) * 5.) - 5. |> int

                    //                    printfn $"tagValue={tagValue} tagIndex={tagIndex}"

                    ConsoleFlag.fg.[Math.Min (ConsoleFlag.fg.Length - 1, Math.Max (0, tagIndex))]
                    $"""[{deviceTag} {DateTime.Now |> DateTime.format "HH:mm:ss"}]"""
                    ConsoleFlag.reset
                    yield! result
                |]
                |> String.concat " "

            logFn output

    let inline log fn = logWithFn (fun x -> printfn $"{x}") fn

    let inline logArray (fn: unit -> _ []) = logWithFn (fun x -> printfn $"{x}") fn
    let inline elog fn = logWithFn (fun x -> eprintfn $"{x}") fn

    let inline logFiltered newValue fn =
        log
            (fun () ->
                if (string newValue).StartsWith "Ping " then
                    null
                else
                    let result: string = fn ()

                    if result.Contains "devicePing" then
                        null
                    else
                        [|
                            result
                        |])

    let inline consoleLog (x: _ []) = emitJsExpr x "console.log(...$0)"
    let inline consoleError x = Browser.Dom.console.error x

    type LogLevel =
        | Trace = 0
        | Debug = 1
        | Info = 2
        | Warning = 3
        | Error = 4
        | Critical = 5

    let DEFAULT_LOG_LEVEL = if isDebug () then LogLevel.Debug else LogLevel.Info

    type LogFn = (unit -> string) -> unit

    type Logger =
        {
            Trace: LogFn
            Debug: LogFn
            Info: LogFn
            Warning: LogFn
            Error: LogFn
        }

    let logIf currentLogLevel logLevel (fn: unit -> string) =
        if currentLogLevel <= logLevel then
            let result = fn ()

            if result |> Option.ofObjUnbox |> Option.isSome then
                logArray
                    (fun () ->
                        [|
                            match logLevel with
                            | LogLevel.Trace -> ConsoleFlag.fgBlack
                            | LogLevel.Debug -> ConsoleFlag.fgGreen
                            | LogLevel.Info -> ConsoleFlag.fgWhite
                            | LogLevel.Warning -> ConsoleFlag.fgYellow
                            | LogLevel.Error -> ConsoleFlag.fgRed
                            | LogLevel.Critical -> ConsoleFlag.fgMagenta
                            | _ -> ConsoleFlag.fgWhite
                            $"[{Enum.name logLevel}]"
                            ConsoleFlag.fgWhite
                            result
                        |])

    type Logger with





        static member inline Create currentLogLevel =
            let log = logIf currentLogLevel

            {
                Trace = log LogLevel.Trace
                Debug = log LogLevel.Debug
                Info = log LogLevel.Info
                Warning = log LogLevel.Warning
                Error = log LogLevel.Error
            }

        static member inline Default = Logger.Create DEFAULT_LOG_LEVEL

    module Logger =
        let mutable lastLogger = Logger.Default

        let inline getLogger () = lastLogger // |> Option.defaultValue Logger.Default

    let inline logTrace fn = Logger.getLogger().Trace fn
    let inline logDebug fn = Logger.getLogger().Debug fn
    let inline logInfo fn = Logger.getLogger().Info fn
    let inline logWarning fn = Logger.getLogger().Warning fn
    let inline logError fn = Logger.getLogger().Error fn

    logInfo (fun () -> $"Dom. deviceInfo={JS.JSON.stringify deviceInfo}")


    let inline exited () =
        if not deviceInfo.IsTesting then
            false
        else
            Browser.Dom.window?exit = true

    let rec waitFor fn =
        async {
            if exited () then
                return (unbox null)
            else
                let ok = fn ()

                if ok then
                    return ()
                else
                    printfn "waitFor: false. waiting..."

                    do! Js.sleep 100
                    return! waitFor fn
        }

    let rec waitForObject fn =
        async {
            if exited () then
                return (unbox null)
            else
                let! obj = fn ()

                match box obj with
                | null ->
                    printfn "waitForObject: null. waiting..."

                    do! Js.sleep 100
                    return! waitForObject fn
                | _ -> return obj
        }

    let rec waitForSome fn =
        async {
            if exited () then
                return (unbox null)
            else
                let! obj = fn ()

                match obj with
                | Some obj -> return obj
                | None ->
                    if deviceInfo.IsTesting then
                        do! Js.sleep 0
                    else
                        logInfo (fun () -> $"waitForSome: none. waiting... {fn.ToString ()}")

                        do! Js.sleep 100

                    return! waitForSome fn
        }

    let inline download content fileName contentType =
        let a = Browser.Dom.document.createElement "a"

        let file =
            Browser.Blob.Blob.Create (
                [|
                    content
                |],
                { new BlobPropertyBag with
                    member _.``type`` = contentType
                    member _.endings = BlobEndings.Transparent

                    member _.``type``
                        with set value = ()

                    member _.endings
                        with set value = ()
                }
            )

        a?href <- Browser.Url.URL.createObjectURL file
        a?download <- fileName
        a.click ()
        a.remove ()

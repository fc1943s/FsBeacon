namespace FsUi.Components

open Fable.Core
open FsStore
open FsUi.State
open FsStore.Hooks
open Feliz
open FsJs
open FsUi.Bindings


module ThemeWrapper =
    [<ReactComponent>]
    let ThemeWrapper themeAtom children =
        let theme = Store.useValue (themeAtom |> Option.defaultValue Store.emptyAtom)
        let darkMode = Store.useValue Atoms.Ui.darkMode

        let newTheme =
            React.useMemo (
                (fun () -> Ui.react.extendTheme (JsInterop.toPlainJsObj ({|  |} ++ theme))),
                [|
                    box theme
                |]
            )

        //        printfn $"ThemeLoader newTheme={JS.JSON.stringify newTheme} theme={theme}"

        Profiling.addTimestamp (fun () -> $"{nameof FsUi} | ThemeWrapper [ render ] darkMode={darkMode}")

        Ui.provider
            (fun x -> x.theme <- newTheme)
            [
                (if darkMode then Ui.darkMode else Ui.lightMode)
                    (fun _ -> ())
                    [
                        yield! children
                    ]
            ]

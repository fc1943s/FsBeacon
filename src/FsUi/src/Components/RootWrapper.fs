namespace FsUi.Components

open Fable.Core
open FsStore
open FsUi.State
open Fable.React
open Feliz
open Feliz.Router
open FsJs
open FsUi.Bindings


module RootWrapper =
    [<ReactComponent>]
    let ThemeLoader themeAtom children =
        let theme = Store.useValue (themeAtom |> Option.defaultValue Store.emptyAtom)
        let darkMode = Store.useValue Atoms.Ui.darkMode

        Profiling.addCount "ThemeLoader().render"

        let newTheme =
            React.useMemo (
                (fun () -> Ui.react.extendTheme (JsInterop.toPlainJsObj ({|  |} ++ theme))),
                [|
                    box theme
                |]
            )

//        printfn $"ThemeLoader newTheme={JS.JSON.stringify newTheme} theme={theme}"

        Ui.provider
            (fun x -> x.theme <- newTheme)
            [
                (if darkMode then Ui.darkMode else Ui.lightMode)
                    (fun _ -> ())
                    [
                        React.router [
                            router.children [ yield! children ]
                        ]
                    ]
            ]

    [<ReactComponent>]
    let RootWrapper themeAtom children =
        Profiling.addCount "RootWrapper().render"

        React.strictMode [
            Store.provider [
                React.suspense (
                    [
                        React.ErrorBoundary [
                            ThemeLoader
                                themeAtom
                                [
                                    yield! children
                                ]
                        ]
                    ],
                    Html.div [
                        prop.className "static"
                        prop.children [ str "Loading..." ]
                    ]
                )
            ]
        ]

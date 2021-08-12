namespace FsUi.Components

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

        UI.provider
            (fun x -> x.theme <- theme)
            [
                (if darkMode then UI.darkMode else UI.lightMode)
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

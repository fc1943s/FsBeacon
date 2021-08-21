namespace FsUi.Components

open FsStore
open Fable.React
open Feliz
open FsJs
open FsUi.Bindings


module RootWrapper =
    [<ReactComponent>]
    let RootWrapper themeAtom children =
        Profiling.addCount "RootWrapper.render"

        React.strictMode [
            Store.provider [
                React.suspense (
                    [
                        React.ErrorBoundary [
                            GunObserver.GunObserver ()
                            RouterObserver.RouterWrapper [
                                ThemeWrapper.ThemeWrapper
                                    themeAtom
                                    [
                                        yield! children
                                    ]
                            ]
                        ]
                    ],
                    Ui.box
                        (fun x -> x.className <- "static")
                        [
                            str "Loading..."
                        ]
                )
            ]
        ]

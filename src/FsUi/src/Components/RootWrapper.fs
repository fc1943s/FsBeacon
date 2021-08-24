namespace FsUi.Components

open FsStore
open Fable.React
open Feliz
open FsJs
open FsStore.Bindings
open FsUi.Bindings


module RootWrapper =
    [<ReactComponent>]
    let RootWrapper themeAtom children =
        Profiling.addTimestamp $"{nameof FsUi} | RootWrapper [ render ] "

        React.strictMode [
            Jotai.jotai.provider [
                React.suspense (
                    [
                        React.ErrorBoundary [
                            GunObserver.GunObserver ()
                            RouterObserverWrapper.RouterObserverWrapper [
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

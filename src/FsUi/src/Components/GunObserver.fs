namespace FsUi.Components

open Fable.Core
open Feliz
open FsStore
open FsStore.Hooks
open FsJs
open FsStore.Bindings
open FsStore.State
open FsUi.Bindings
open FsUi.Hooks
open Fable.React


module GunObserver =

    [<ReactComponent>]
    let GunObserver () =
        let logger = Store.useValue Selectors.logger
        let gun = Store.useValue Selectors.Gun.gun
        let store = Store.useStore ()
        let isMountedRef = React.useIsMountedRef ()

        Profiling.addTimestamp $"{nameof FsUi} | GunObserver [ render ] isMountedRef={isMountedRef.current}"

        // TODO: arr deps warning is not working with files in this package (only on template main)
        React.useEffect (
            (fun () ->
                gun.on (
                    Gun.GunEvent "auth",
                    (fun () ->
//                        JS.setTimeout
//                            (fun () ->
                                promise {
                                    if isMountedRef.current then
                                        let! _getter, setter = store ()
                                        //                                ()

                                        Profiling.addTimestamp
                                            $"{nameof FsUi} | GunObserver [ render / useEffect / setTimeout ] triggering."

                                        Atom.change setter Atoms.gunTrigger ((+) 1)
                                        Atom.change setter Atoms.hubTrigger ((+) 1)

                                        logger.Debug (fun () -> "GunObserver.render. triggered.  ")
                                    else
                                        logger.Debug (fun () -> "GunObserver.render. already disposed")
                                }
                                |> Promise.start
//                                )
//                            0
//                        |> ignore
                        )
                )),
            [|
                box isMountedRef
                box gun
                box store
            |]
        )

        nothing

namespace FsUi.Components

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
                        if isMountedRef.current then
                            promise {
                                ()
                                //                                let user = gun.user ()

                                //                                let! alias =
//                                    match user.__.sea, user.is with
//                                    | _,
//                                      Some {
//                                               alias = Some (Gun.GunUserAlias.Alias (Gun.Alias (String.Valid alias)))
//                                           } ->
//                                        logger.Debug
//                                            (fun () ->
//                                                $"GunObserver.render: .on(auth) effect. setUsername. alias={alias}")
//
//                                        let keys = user.__.sea
//
//                                        match keys with
//                                        | Some _keys -> Some (Gun.Alias alias) |> Promise.lift
//                                        | None -> failwith $"GunObserver.render: No keys found for user {alias}"
//                                    | Some ({
//                                                pub = Some (Gun.Pub (String.Valid _))
//                                            } as keys),
//                                      _ ->
//                                        promise {
//                                            let! data = Gun.radQuery gun
//                                            let! alias = Gun.userDecode<Gun.Alias> keys data
//                                            return alias
//                                        }
//                                    | _ ->
//                                        match Dom.window () with
//                                        | Some window -> window?gun <- gun
//                                        | None -> ()
//
//                                        // @@@@ getImmutableUsername pub
//
//                                        logger.Debug
//                                            (fun () ->
//                                                $"GunObserver.render: Auth occurred without username.
//                                        user.is={user.is |> Js.objectKeys}
//                                        user.is={user.is |> JS.JSON.stringify} ")
//
//                                        Promise.lift None
//
                                let! _getter, setter = store ()
                                //                                ()

                                Atom.change setter Selectors.Gun.gunTrigger ((+) 1)
                                Atom.change setter Selectors.Hub.hubTrigger ((+) 1)

                                Profiling.addTimestamp $"{nameof FsUi} | GunObserver [ render / useEffect ] triggered."
                                logger.Debug (fun () -> "GunObserver.render. triggered.  ")
                            }
                            |> Promise.start
                        else
                            logger.Debug (fun () -> "GunObserver.render. already disposed"))
                )),
            [|
                box isMountedRef
                box gun
                box store
            |]
        )

        nothing

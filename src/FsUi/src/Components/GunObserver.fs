namespace FsUi.Components

open Fable.Core
open FsCore
open Fable.Core.JsInterop
open Feliz
open FsJs
open FsStore
open FsStore.Bindings
open FsUi.Bindings
open FsUi.Hooks
open Fable.React


module GunObserver =

    [<ReactComponent>]
    let GunObserver () =
        let logger = Store.useValue Selectors.logger
        let gun = Store.useValue Selectors.Gun.gun
        //        let appKeys = Gun.gunHooks.useGunKeys Browser.Dom.window?SEA (fun () -> null) false

        //        let gunState =
//            Gun.gunHooks.useGunState
//                (gunNamespace.ref.get ("fluke"))
//                {|
//                    appKeys = gunKeys
//                    sea = Browser.Dom.window?SEA
//                |}
//
//        printfn $"GunObserver. gunState={JS.JSON.stringify gunState}"
//        printfn "GunObserver. setted dom.gunState"
//        Browser.Dom.window?gunState <- gunState

        //        const [appKeys, setAppKeys] = useGunKeys(
//          sea,
//          () =>
//            JSON.parse(localStorage.getItem('existingKeysInLocalStorage')) || null,
//        );
//        const [user, isLoggedIn] = useGunKeyAuth(gun, appKeys);

        //        let setUsername = Recoil.useSetState Atoms.username

        //        let setSessionRestored = Store.useSetState Atoms.Session.sessionRestored

        logger.Debug (fun () -> "GunObserver.render: Constructor")


        //        React.useEffect (
//            (fun () ->
//                //                let recall = Browser.Dom.window.sessionStorage.getItem "recall"
////                printfn $"recall {recall}"
////
////                match recall with
////                | null
////                | "" -> setSessionRestored true
////                | _ -> ()
////
////                printfn "before recall"
////
////                try
////                    if true then
////                        gunNamespace.ref.recall (
////                            {| sessionStorage = true |},
////                            (fun ack ->
////                                match ack.put with
////                                | Some put -> setUsername (Some (UserInteraction.Username put.alias))
////                                | None -> printfn "Empty ack"
////
////                                setKeys (Some ack.sea)
////
////                                setSessionRestored true
////
////                                printfn "ACK:"
////                                Browser.Dom.console.log ack
////                                Browser.Dom.window?recallAck <- ack
////                                Dom.set "ack" ack)
////                        )
////                with ex -> printfn $"ERROR: {ex}"
//
//                //                printfn "after recall"
//
//                printfn "before newRecall"
//
//                printfn $"gunKeys={gunKeys |> Some |> JS.objectKeys}"
//                setSessionRestored true
//
//                printfn "after newRecall"),
//            [|
//                box gunNamespace
//                box  setSessionRestored
//                box gunKeys
//            |]
//        )

        let callbacks = Store.useCallbacks ()

        React.useDisposableEffect (
            (fun disposed ->
                gun.on (
                    Gun.GunEvent "auth",
                    (fun () ->
                        if not disposed then
                            promise {
                                let user = gun.user ()

                                match user.is with
                                | Some {
                                           alias = Some (Gun.GunUserAlias.Alias (Gun.Alias (String.ValidString username)))
                                       } ->
                                    logger.Debug
                                        (fun () ->
                                            $"GunObserver.render: .on(auth) effect. setUsername. username={username}")

                                    let keys = user.__.sea

                                    match keys with
                                    | Some _keys -> ()
                                    //                                    setUsername (Some (Username username))
//                                    setGunKeys keys
                                    | None -> failwith $"GunObserver.render: No keys found for user {username}"

                                //                                gunState.put ({| username = username |} |> toPlainJsObj)
                                //                                |> Promise.start
                                //                                setUsername (Some (UserInteraction.Username username))
                                | Some {
                                           pub = Some (Gun.Pub (String.ValidString pub))
                                       } ->
                                    match Dom.window () with
                                    | Some window -> window?gun <- gun
                                    | None -> ()

                                    let _a =
                                        user
                                            .get(Gun.GunNodeSlice $"#{nameof Gun.data}")
                                            .once (fun a b -> logger.Debug (fun () -> $"_a. a={a} b={b}"))

                                    let _b =
                                        user
                                            .get(Gun.GunNodeSlice $"#{nameof Gun.data}")
                                            .get(Gun.RadQuery (Gun.radQuery (Gun.Pub pub)))
                                            .once (fun a b -> logger.Debug (fun () -> $"_b. a={a} b={b}"))

                                    user
                                        .get(Gun.GunNodeSlice $"#{nameof Gun.data}")
                                        .get(Gun.RadQuery (Gun.radQuery (Gun.Pub pub)))
                                        .map()
                                        .once (fun encryptedUsername k ->
                                            logger.Debug
                                                (fun () ->
                                                    $"@@@@@@@@@ encryptedUsername={encryptedUsername} k={k} pub={pub}")

                                            match encryptedUsername with
                                            | Gun.GunValue.NodeReference gunNodeSlice ->
                                                gun
                                                    .user(Gun.Pub pub)
                                                    .get(Gun.GunNodeSlice (nameof Gun.data))
                                                    .get(gunNodeSlice)
                                                    .once (fun a b ->
                                                        logger.Debug (fun () -> $"gun once! a={a} b={b}")
                                                        ())
                                            | _ -> ())

                                    let username = pub

                                    logger.Debug
                                        (fun () -> $"GunObserver.render: fetched username: {username} (still a key)")
                                | _ ->
                                    match Dom.window () with
                                    | Some window -> window?gun <- gun
                                    | None -> ()

                                    // @@@@ getImmutableUsername pub

                                    logger.Debug
                                        (fun () ->
                                            $"GunObserver.render: Auth occurred without username.
                                    user.is={user.is |> Js.objectKeys}
                                    user.is={user.is |> JS.JSON.stringify} ")

                                let! _getter, setter = callbacks ()
                                ()

                                Store.change setter Atoms.gunTrigger ((+) 1)
                                Store.change setter Atoms.hubTrigger ((+) 1)
                            }
                            |> Promise.start
                        else
                            logger.Debug (fun () -> $"GunObserver.render: already disposed gun={gun}"))
                )),
            [|
                box gun
                box callbacks
            |]
        )

        nothing

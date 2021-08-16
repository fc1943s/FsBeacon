namespace FsUi.Hooks

open Fable.Extras
open Fable.Core
open FsJs
open FsStore
open FsStore.Bindings


module rec Auth =
    let inline useLogout () =
        Store.useCallbackRef
            (fun getter setter () ->
                promise {
                    printfn "useLogout(). before leave"
                    let gunUser = Store.value getter Selectors.Gun.gunUser
                    gunUser.leave ()
                    Store.change setter Atoms.gunTrigger ((+) 1)
                    Store.change setter Atoms.hubTrigger ((+) 1)
                })

    let inline useSignIn () =
        Store.useCallbackRef
            (fun getter _setter (username, password) ->
                promise {
                    let gunUser = Store.value getter Selectors.Gun.gunUser

                    let! ack =
                        match username, password with
                        | "", keys ->
                            printfn "keys sign in"

                            let keys =
                                try
                                    keys |> Json.decode<Gun.GunKeys>
                                with
                                | ex ->
                                    printfn $"keys decode error: {ex.Message}"
                                    Gun.GunKeys.Default

                            Gun.authKeys gunUser keys

                        | username, password ->
                            printfn "user/pass sign in"
                            Gun.authUser gunUser (Gun.Alias username) (Gun.Pass password)

                    match ack with
                    | { err = None } ->
                        let keys = gunUser.__.sea

                        match keys with
                        | Some keys ->
                            //                        do! Promise.sleep 100
//                            Store.change setter Atoms.gunTrigger ((+) 1)
//                            Store.change setter Atoms.hubTrigger ((+) 1)
                            return Ok (Gun.Alias username, keys)
                        | None -> return Error $"No keys found for user {username} after sign in"
                    | { err = Some error } -> return Error error
                })

    let inline useChangePassword () =
        Store.useCallbackRef
            (fun getter setter (password, newPassword) ->
                promise {
                    let alias = Store.value getter Selectors.Gun.alias
                    let gunUser = Store.value getter Selectors.Gun.gunUser

                    match alias with
                    | Some (Gun.Alias alias) ->
                        let! ack =
                            Gun.changeUserPassword gunUser (Gun.Alias alias) (Gun.Pass password) (Gun.Pass newPassword)

                        return!
                            promise {
                                match ack with
                                | { ok = Some 1; err = None } ->
                                    Store.change setter Atoms.gunTrigger ((+) 1)
                                    Store.change setter Atoms.hubTrigger ((+) 1)
                                    return Ok ()
                                | { err = Some error } -> return Error error
                                | _ -> return Error $"invalid ack {JS.JSON.stringify ack}"
                            }
                    | _ -> return Error "Invalid username"
                })

    let inline useDeleteUser () =
        let logout = useLogout ()

        Store.useCallbackRef
            (fun getter _ password ->
                promise {
                    let alias = Store.value getter Selectors.Gun.alias

                    match alias with
                    | Some (Gun.Alias alias) ->
                        let gunUser = Store.value getter Selectors.Gun.gunUser

                        let! ack = Gun.deleteUser gunUser (Gun.Alias alias) (Gun.Pass password)
                        printfn $"ack={JS.JSON.stringify ack}"

                        return!
                            promise {
                                match ack with
                                | { ok = Some 0; err = None } ->
                                    do! logout ()
                                    return Ok ()
                                | { err = Some error } -> return Error error
                                | _ -> return Error $"invalid ack {JS.JSON.stringify ack}"
                            }
                    | _ -> return Error "Invalid username"
                })

    let inline useSignUp () =
        let signIn = useSignIn ()

        Store.useCallbackRef
            (fun getter _setter (username, password) ->
                promise {
                    if username = "" || password = "" then
                        return Error "Required fields"
                    elif JSe
                             .RegExp(@"^[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*$")
                             .Test username
                         |> not then
                        return Error "Invalid email address"
                    else
                        let gunUser = Store.value getter Selectors.Gun.gunUser

                        printfn $"Auth.useSignUp. gunUser.is={gunUser.is |> Js.objectKeys}"

                        let! ack = Gun.createUser gunUser (Gun.Alias username) (Gun.Pass password)

                        printfn $"Auth.useSignUp. Gun.createUser signUpAck={JS.JSON.stringify ack}"

                        return!
                            promise {
                                match ack with
                                | {
                                      err = None
                                      ok = Some 0
                                      pub = Some _
                                  } ->
                                    do! Gun.putPublicHash (gunUser :?> Gun.Types.IGunChainReference) username


                                    //            match! Gun.put (gun.get("#").get hash) data with
//            | true -> return Ok hash
//            | false -> return Error $"put error. data={data} hash={hash}"

                                    //                                    "hash#atomPath"
                                    //                                    "atomPath#hash"
//                                    setImmutableUsername pub username

                                    match! signIn (username, password) with
                                    | Ok (username, keys) -> return Ok (username, keys)
                                    | Error error -> return Error error
                                | { err = Some err } -> return Error err
                                | _ -> return Error $"Invalid ack: {JS.JSON.stringify ack}"
                            }
                })

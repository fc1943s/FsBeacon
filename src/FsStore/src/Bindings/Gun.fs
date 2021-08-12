namespace FsStore.Bindings

open FsCore
open FsJs
open Fable.SignalR
open Fable.Core
open Fable.Core.JsInterop
open System


module Gun =
    [<Erase>]
    type Pub = Pub of string

    [<Erase>]
    type Epub = Epub of string

    [<Erase>]
    type Priv = Priv of string

    [<Erase>]
    type Epriv = Epriv of string

    [<Erase>]
    type GunKeys =
        {
            pub: Pub option
            epub: Epub option
            priv: Priv option
            epriv: Epriv option
        }
        static member inline Default =
            {|
                pub = None
                epub = None
                priv = None
                epriv = None
            |}
            |> unbox<GunKeys>

    type UserResult =
        {

            err: string option
            ok: int option
            pub: string option
            ``#``: string option
            [<Emit("@")>]
            at: string option
            wait: bool
        }


    type IGunUserPub =
        {
            alias: GunUserAlias option
            pub: Pub option
        }

    and [<Erase>] Alias = Alias of alias: string

    and [<Erase; RequireQualifiedAccess>] GunUserAlias =
        | Alias of alias: Alias
        | GunKeys of keys: GunKeys


    [<Erase>]
    type GunNodeSlice = GunNodeSlice of value: string

    type GunNodeSlice with
        static member inline Value (GunNodeSlice value) = value

    [<Erase>]
    type GunPeer = GunPeer of url: string

    type GunPeer with
        static member inline Value (GunPeer url) = url

    [<Erase>]
    type ValueHash = ValueHash of hash: string

    [<Erase>]
    type CryptoName = CryptoName of name: string

    [<Erase>]
    type EncryptedValue = EncryptedValue of value: string

    [<Erase>]
    type EncryptedSignedValue = EncryptedSignedValue of value: string

    [<Erase; RequireQualifiedAccess>]
    type GunValue =
        | NodeReference of GunNodeSlice
        | EncryptedSignedValue of EncryptedSignedValue

    [<Erase>]
    type DecryptedValue = DecryptedValue of value: string

    [<Erase>]
    type Pass = Pass of pass: string

    [<Erase>]
    type GunEvent = GunEvent of gunEvent: string

    [<Erase>]
    type RadQuery = RadQuery of query: {| ``.``: {| ``*``: Pub |} |}


    module rec Types =
        type IGunNode =
            abstract get : RadQuery -> IGunChainReference
            abstract get : GunNodeSlice -> IGunChainReference
            abstract set : GunValue -> IGunChainReference

        //        module GunOps =
//            let inline private (|HasAuth|) x = (^a: (member A : string) x)

        type IGunUser =
            inherit IGunNode
            abstract create : alias: Alias * pass: Pass * cb: (UserResult -> unit) -> unit
            abstract delete : alias: Alias * pass: Pass * cb: (UserResult -> unit) -> unit
            abstract auth : alias: Alias * pass: Pass * cb: (UserResult -> unit) * ?opt: {| change: Pass |} -> unit
            abstract auth : keys: GunKeys * cb: (UserResult -> unit) * ?opt: {| change: Pass |} -> unit

            [<Emit("$0._")>]
            abstract __ : {| sea: GunKeys option |}

            abstract leave : unit -> unit

            abstract recall :
                {| sessionStorage: bool |}
                * System.Func<{| put: {| alias: Alias |} option
                                 sea: GunKeys |}, unit> ->
                unit

            abstract is : IGunUserPub option

        type PutAck =
            {
                err: int option
                ok: int option
                ``#``: string option
                [<Emit("@")>]
                at: string option
            }

        type PutNode =
            {
                err: int option
                off: (unit -> unit) option
            }

        type IGunChainReference =
            inherit IGunNode
            abstract map : unit -> IGunChainReference
            abstract off : unit -> IGunChainReference
            abstract back : unit -> IGunChainReference
            abstract on : (GunValue -> GunNodeSlice -> unit) -> unit
            abstract once : (GunValue -> GunNodeSlice -> unit) -> unit
            abstract on : event: GunEvent * (unit -> unit) -> unit
            abstract put : GunValue -> (PutAck -> PutNode -> unit) -> IGunChainReference
            abstract user : unit -> IGunUser
            abstract user : Pub -> IGunUser
    //        abstract once : (string -> unit) -> unit
    //        abstract set : string -> IGunChainReference
    open Types


    type ISEA =
        abstract encrypt : data: DecryptedValue -> keys: GunKeys -> JS.Promise<EncryptedValue>
        abstract sign : data: EncryptedValue -> keys: GunKeys -> JS.Promise<EncryptedSignedValue>
        abstract verify : data: EncryptedSignedValue -> pub: Pub -> JS.Promise<EncryptedValue option>
        abstract decrypt : data: EncryptedValue -> keys: GunKeys -> JS.Promise<DecryptedValue>

        abstract work :
            data: GunValue ->
            keys: GunKeys option ->
            x: unit option ->
            crypto: {| name: CryptoName option |} option ->
            JS.Promise<ValueHash>

    type GunProps =
        {
            peers: GunPeer [] option
            radisk: bool option
            localStorage: bool option
            multicast: bool option
        }

    //    match JS.window id with
//    | Some _ -> ()
//    | None -> importAll "gun"

    let gun: GunProps -> IGunChainReference =
        if Dom.deviceInfo.IsTesting then
            importDefault "gun/gun"
        else
            importAll "gun/lib/radix"
            importAll "gun/lib/radisk"
            importAll "gun/lib/store"
            importAll "gun/lib/rindexed"

            importDefault "gun"


    importAll "gun/sea"
    importAll "gun/lib/promise"

    let sea: ISEA = emitJsExpr () "Gun.SEA"

    let inline createUser (user: IGunUser) username password =
        Promise.create
            (fun res err ->
                try
                    user.create (username, password, res)
                with
                | ex ->
                    printfn $"createUser error: {ex}"
                    err ex)

    let inline authUser (user: IGunUser) username password =
        Promise.create
            (fun res err ->
                try
                    user.auth (username, password, res)
                with
                | ex ->
                    printfn "authUser error: {ex}"
                    err ex)

    let inline authKeys (user: IGunUser) keys =
        Promise.create
            (fun res err ->
                try
                    user.auth (keys, res)
                with
                | ex ->
                    printfn "authKeys error: {ex}"
                    err ex)

    let inline changeUserPassword (user: IGunUser) username password newPassword =
        Promise.create
            (fun res err ->
                try
                    user.auth (username, password, res, {| change = newPassword |})
                with
                | ex ->
                    printfn "changeUserPassword error: {ex}"
                    err ex)

    let inline deleteUser (user: IGunUser) username password =
        Promise.create
            (fun res err ->
                try
                    user.delete (username, password, res)
                with
                | ex ->
                    printfn "deleteUser error: {ex}"
                    err ex)


    let inline userDecode<'TValue> (keys: GunKeys) data =
        promise {
            let! decrypted =
                promise {
                    match keys with
                    | { pub = Some pub } ->
                        try
                            let! verified = sea.verify data pub

                            match verified with
                            | Some encryptedGunValue ->
                                let! decrypted = sea.decrypt encryptedGunValue keys
                                return Some decrypted
                            | None -> return None
                        with
                        | ex ->
                            Dom.consoleError ("userDecode decrypt exception", ex, data)
                            return None
                    | _ -> return None
                }

            let decoded =
                try
                    match decrypted with
                    | Some (DecryptedValue decrypted) -> decrypted |> Json.decode<'TValue option>
                    | None ->
                        Dom.log (fun () -> $"userDecode decrypt empty. decrypted={decrypted} data={data}")
                        JS.undefined
                with
                | ex ->
                    Dom.consoleError ("userDecode decode error. ex=", ex, "data=", data, "decrypted=", decrypted)
                    None

            return decoded
        }

    let inline userEncode<'TValue> (gun: IGunChainReference) (value: 'TValue) =
        promise {
            try
                let user = gun.user ()
                let keys = user.__.sea

                match keys with
                | Some keys ->
                    let json =
                        value
                        |> Json.encode<'TValue>
                        |> Json.encode<string>

                    //                    printfn $"userEncode value={value} json={json}"
//
                    let! encrypted = sea.encrypt (DecryptedValue json) keys

                    let! signed = sea.sign encrypted keys
                    //                    Dom.log (fun () -> $"userEncode. json={json} encrypted={encrypted} signed={signed}")
                    return signed
                | None -> return failwith $"No keys found for user {user.is}"
            with
            | ex ->
                Dom.consoleError ("[exception4]", ex, value)
                return raise ex
        }

    type Serializer<'T> = ('T -> string) * (string -> 'T)

    let inline defaultSerializer<'T> : Serializer<'T> = Json.encode<'T>, Json.decode<'T>

    let inline put (gun: IGunChainReference) (value: GunValue) =
        Promise.create
            (fun res _err ->
                let newValue = value
                //                let newValue = if value = Dom.undefined && not Dom.jestWorkerId then null else value
                gun.put
                    newValue
                    (fun ack _node ->
                        if ack.ok = Some 1 && ack.err.IsNone then
                            res true
                        else

                            match Dom.window () with
                            | Some window ->
                                if window?Cypress = null then
                                    Dom.consoleError $"Gun.put error. newValue={newValue} ack={JS.JSON.stringify ack} "
                            | None -> ()

                            res false)
                |> ignore)

    let rec data = nameof data

    let inline putPublicHash (gun: IGunChainReference) value =
        promise {
            let! encryptedValue = userEncode gun value

            let user = gun.user ()

            printfn $"#1 {JS.JSON.stringify user.is}"

            match user.is with
            | Some { pub = Some (Pub pub) } ->
                user
                    .get(GunNodeSlice (nameof data))
                    .set(GunValue.EncryptedSignedValue encryptedValue)
                    .on (fun data key ->
                        (promise {
                            let! hash = sea.work data None None (Some {| name = Some (CryptoName "SHA-256") |})
                            printfn $"#2 data={data} key={key} hash={hash}"

                            let node =
                                gun
                                    .get(GunNodeSlice $"#{nameof data}")
                                    .get (GunNodeSlice $"{pub}#{hash}")

                            let! putResult = put node (GunValue.NodeReference key)

                            if not putResult then
                                eprintfn $"put public hash error key={key} pub={pub} hash={hash}"
                         }
                         |> Promise.start))
            | _ -> ()
        }

    let inline subscribe (gun: IGunChainReference) fn =
        gun.on
            (fun data (GunNodeSlice key) ->
                Dom.log
                    (fun () ->
                        if key = "devicePing" then
                            null
                        else
                            $"subscribe.on() data. batching...data={data} key={key}")

                fn data)

        Object.newDisposable
            (fun () ->
                printfn "subscribe.on() data. Dispose promise observable."
                gun.off () |> ignore)
        |> Promise.lift


    let inline batchData<'T> (fn: int64 * 'T -> JS.Promise<IDisposable>) (data: 'T) =
        Batcher.batch (Batcher.BatchType.Data (data, DateTime.Now.Ticks, fn))

    let inline batchKeys map fn data =
        let fn = map >> fn
        Batcher.batch (Batcher.BatchType.KeysFromServer (data, DateTime.Now.Ticks, fn))

    let inline batchSubscribe gunAtomNode fn =
        let fn () = subscribe gunAtomNode (batchData fn)
        Batcher.batch (Batcher.BatchType.Subscribe fn)

    let inline batchSet gunAtomNode fn =
        let fn () = subscribe gunAtomNode (batchData fn)
        Batcher.batch (Batcher.BatchType.Subscribe fn)

    let inline hubSubscribe<'A, 'R> (hub: HubConnection<'A, 'A, _, 'R, 'R>) action fn onError =
        promise {
            let! stream = hub.streamFrom action |> Async.StartAsPromise

            let subscription =
                stream.subscribe
                    {
                        next = fun (msg: 'R) -> fn msg
                        complete =
                            fun () ->
                                Dom.log
                                    (fun () -> $"[hubSubscribe.complete() HUB stream subscription] action={action} ")
                        error =
                            fun err ->
                                Dom.consoleError (
                                    $"[hubSubscribe.error() HUB stream subscription] action={action} ",
                                    err
                                )

                                onError err
                    }

            return subscription
        }

    let inline batchHubSubscribe (hub: HubConnection<'A, 'A, _, 'R, 'R>) action fn onError =
        let fn () =
            hubSubscribe hub action (batchData fn) onError

        Batcher.batch (Batcher.BatchType.Subscribe fn)

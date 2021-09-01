namespace FsStore.Bindings

open FsCore
open FsJs
open Fable.SignalR
open Fable.Core
open Fable.Core.JsInterop


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
    type AtomKeyFragment = AtomKeyFragment of string

    [<Erase>]
    type GunPeer = GunPeer of url: string

    [<Erase>]
    type ValueHash = ValueHash of hash: string

    [<Erase>]
    type CryptoName = CryptoName of name: string

    [<Erase>]
    type EncryptedValue = EncryptedValue of value: string

    [<Erase>]
    type EncryptedSignedValue = EncryptedSignedValue of value: string

    [<Erase>]
    type DecryptedValue<'T> = DecryptedValue of value: 'T

    [<Erase; RequireQualifiedAccess>]
    type GunValue<'T> =
        | NodeReference of AtomKeyFragment
        | EncryptedSignedValue of EncryptedSignedValue
        | DecryptedValue of DecryptedValue<'T>

    [<Erase>]
    type Pass = Pass of pass: string

    [<Erase>]
    type GunEvent = GunEvent of gunEvent: string

    [<Erase>]
    type RadQuery = RadQuery of query: {| ``.``: {| ``*``: Pub |} |}


    type AtomKeyFragment with
        static member inline Value (AtomKeyFragment value) = value

    type GunPeer with
        static member inline Value (GunPeer url) = url

    let inline pubRadQuery (Pub pub) : {| ``.``: {| ``*``: Pub |} |} = emitJsExpr pub "{'.':{'*':$0}}"


    module rec Types =
        type IGunNode =
            abstract get : RadQuery -> IGunChainReference
            abstract get : AtomKeyFragment -> IGunChainReference
            abstract set : GunValue<'T> -> IGunChainReference

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
            abstract on : (GunValue<'T> -> AtomKeyFragment -> JS.Promise<unit>) -> unit
            abstract once : (GunValue<'T> -> AtomKeyFragment -> unit) -> unit
            abstract on : event: GunEvent * (unit -> unit) -> unit
            abstract put : GunValue<'T> -> (PutAck -> PutNode -> unit) -> IGunChainReference
            abstract user : unit -> IGunUser
            abstract user : Pub -> IGunUser
    //        abstract once : (string -> unit) -> unit
    //        abstract set : string -> IGunChainReference
    open Types


    type ISEA =
        abstract encrypt : data: DecryptedValue<'T> -> keys: GunKeys -> JS.Promise<EncryptedValue>
        abstract sign : data: EncryptedValue -> keys: GunKeys -> JS.Promise<EncryptedSignedValue>
        abstract verify : data: EncryptedSignedValue -> pub: Pub -> JS.Promise<EncryptedValue option>
        abstract decrypt : data: EncryptedValue -> keys: GunKeys -> JS.Promise<DecryptedValue<'T>>

        abstract work :
            data: GunValue<'T> ->
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


    let inline userDecode<'TValue> (privateKeys: GunKeys) data =
        promise {
            let! decrypted =
                promise {
                    match privateKeys with
                    | { pub = Some pub } when data |> Option.ofObjUnbox |> Option.isSome ->
                        try
                            let! verified = sea.verify data pub

                            match verified with
                            | Some encryptedGunValue ->
                                let! decrypted = sea.decrypt encryptedGunValue privateKeys
                                return Some decrypted
                            | None -> return None
                        with
                        | ex ->
                            Logger.logError
                                (fun () -> $"userDecode decrypt exception. data={Json.encodeWithNull data} ex={ex}")

                            return None
                    | _ -> return None
                }

            let decoded =
                match decrypted with
                | Some (DecryptedValue decrypted) -> decrypted |> Json.decode<'TValue option>
                | None ->
                    Logger.logDebug
                        (fun () -> $"userDecode decrypt empty. decrypted={decrypted} data={Json.encodeWithNull data}")

                    JS.undefined

            return decoded
        }

    let inline userEncode<'TValue> (keys: GunKeys) (value: 'TValue) =
        promise {
            try
                let json =
                    value
                    |> Json.encode<'TValue>
                    |> Json.encode<string>

                let! encrypted = sea.encrypt (DecryptedValue json) keys
                let! signed = sea.sign encrypted keys

                Logger.logTrace
                    (fun () -> $"userEncode. value={value} json={json} encrypted={encrypted} signed={signed}")

                return signed
            with
            | ex ->
                Logger.logError
                    (fun () -> $"userEncode exception. raising to caller inside promise... ex={ex} value={value}")

                return raise ex
        }

    type Serializer<'T> = ('T -> string) * (string -> 'T)

    let inline defaultSerializer<'T> : Serializer<'T> = Json.encode<'T>, Json.decode<'T>

    let inline put (gun: IGunChainReference) (value: GunValue<'T>) =
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
                                    Logger.logError
                                        (fun () -> $"Gun.put error. newValue={newValue} ack={JS.JSON.stringify ack} ")
                            | None -> ()

                            res false)
                |> ignore)

    let rec data = nameof data

    let inline putPublicHash<'TValue> (gun: IGunChainReference) (value: 'TValue) =
        promise {
            let user = gun.user ()

            match user.__.sea with
            | Some ({
                        priv = Some (Priv (String.Valid _))
                        pub = Some (Pub (String.Valid pub))
                    } as keys) ->

                let dataSlice = AtomKeyFragment (nameof data)
                let node = user.get dataSlice

                //                    GunValue.DecryptedValue (DecryptedValue value)
//                let valueSet = node.set newValue

                let! newValue = userEncode<'TValue> keys value
                printfn $"putPublicHash starting. newValue={newValue} t={jsTypeof newValue}"

                let valueSet = node.set (GunValue.EncryptedSignedValue newValue)

                valueSet.on
                    (fun _data key ->
                        (promise {
                            let! hash =
                                sea.work
                                    (GunValue.NodeReference key)
                                    None
                                    None
                                    (Some {| name = Some (CryptoName "SHA-256") |})

                            let node =
                                gun
                                    .get(AtomKeyFragment $"#{nameof data}")
                                    .get (AtomKeyFragment $"{pub}#{hash}")

                            let! putResult = put node (GunValue.NodeReference key)

                            Logger.logDebug
                                (fun () ->
                                    $"putPublicHash completed. putResult={putResult} key={key} pub={pub} hash={hash}")
                         }))
            | _ -> eprintfn $"invalid key. user.is={JS.JSON.stringify user.is}"
        }

    type RawDataEntry = RawDataEntry of key: string * value: string

    let inline radQuery (gun: IGunChainReference) =
        Promise.create
            (fun res err ->
                try
                    let user = gun.user ()

                    match user.is with
                    | Some {
                               alias = (Some (GunUserAlias.GunKeys { pub = Some pub }))
                           } ->
                        gun
                            .get(AtomKeyFragment $"#{nameof data}")
                            .get(RadQuery (pubRadQuery pub))
                            .map()
                            .once (fun gunValue _nodeSlice ->
                                match gunValue with
                                | GunValue.NodeReference gunNodeSlice ->
                                    gun
                                        .user(pub)
                                        .get(AtomKeyFragment (nameof data))
                                        .get(gunNodeSlice)
                                        .once (fun result _key ->
                                            printfn $"hashData result={result} key={_key}"
                                            res (result |> unbox<EncryptedSignedValue>)
                                            ())
                                | _ -> Logger.logDebug (fun () -> "radQuery gunValue is not nodereference"))
                    | _ -> Logger.logDebug (fun () -> "radQuery. no pub found")
                with
                | ex ->
                    printfn "radQuery error: {ex}"
                    err ex)

    let inline subscribe (gun: IGunChainReference) fn =
        gun.on
            (fun data key ->
                promise {
                    Profiling.addTimestamp
                        (fun () -> $"($$) ---- Gun.subscribe.on() data. batching... key={key} data={data} ")

                    fn (data |> unbox<EncryptedSignedValue>, key)
                })

        Object.newDisposable
            (fun () ->
                Profiling.addTimestamp (fun () -> $"($$) ---- Gun.subscribe.on() data. Dispose promise observable. ")
                gun.off () |> ignore)
        |> Promise.lift


    let inline batchData
        (fn: TicksGuid * EncryptedSignedValue * AtomKeyFragment -> JS.Promise<unit>)
        (ticks: TicksGuid, data: EncryptedSignedValue, AtomKeyFragment key)
        =
        Profiling.addTimestamp (fun () -> $"($$) ---- #B key={key} subscriptionTicks={ticks} data={data} ")

        Batcher.batch (
            Batcher.BatchType.Data (
                ticks,
                data,
                key,
                (fun (ticks, value, key) -> fn (ticks, value, AtomKeyFragment key))
            )
        )

    let inline batchKeys atomType fn (ticks, data) map =
        let fn = map >> fn
        Batcher.batch (Batcher.BatchType.KeysFromServer (atomType, data, ticks, fn))

    let inline batchSubscribe gunAtomNode ticks trigger =
        let inline fn ticks =
            Profiling.addTimestamp (fun () -> $"($$) ---- #1.1 ticks={ticks} gunAtomNode={gunAtomNode} ")

            subscribe
                gunAtomNode
                (fun (value, key) ->
                    Profiling.addTimestamp (fun () -> $"($$) ---- #A key={key} ticks={ticks} value={value} ")
                    batchData trigger (ticks, value, key))

        Profiling.addTimestamp (fun () -> $"($$) ---- #1 ticks={ticks} ")
        Batcher.batch (Batcher.BatchType.Subscribe (ticks, fn))

    let inline batchSet gunAtomNode (ticks, trigger) =
        let inline fn ticks =
            subscribe gunAtomNode (fun (value, key) -> batchData trigger (ticks, value, key))

        Batcher.batch (Batcher.BatchType.Subscribe (ticks, fn))

    let inline hubSubscribe<'A, 'R> (hub: HubConnection<'A, 'A, _, 'R, 'R>) action fn onError =
        promise {
            let! stream = hub.streamFrom action |> Async.StartAsPromise

            let subscription =
                stream.subscribe
                    {
                        next = fun (msg: 'R) -> fn msg
                        complete =
                            fun () ->
                                Logger.logDebug
                                    (fun () -> $"[hubSubscribe.complete() HUB stream subscription] action={action} ")
                        error =
                            fun err ->
                                Logger.logError
                                    (fun () ->
                                        $"[hubSubscribe.error() HUB stream subscription] action={action} err={err}")

                                onError err
                    }

            return subscription
        }

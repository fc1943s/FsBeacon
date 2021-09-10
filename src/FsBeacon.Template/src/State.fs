namespace FsBeacon.Template

open FsJs
open FsCore
open FsCore.BaseModel
open FsStore
open FsStore.Bindings.Gun
open FsStore.Hooks
open FsStore.Model
open FsStore.State
open FsUi.State

#nowarn "40"


module State =
    module rec FsBeacon =
        let storeRoot = StoreRoot (nameof FsBeacon)


    module rec Host =
        [<RequireQualifiedAccess>]
        type AccordionType = | HostComponent


    module Atoms =

        module rec Host =
            let collection = Collection (nameof Host)

            let rec accordionHiddenFlag =
                Atom.Primitives.atomFamily
                    (fun (accordionType: Host.AccordionType) ->
                        Atom.create
                            (ValueAtomPath (
                                FsBeacon.storeRoot,
                                collection,
                                accordionType
                                |> string
                                |> AtomKeyFragment
                                |> List.singleton,
                                (AtomName (nameof accordionHiddenFlag))
                            ))
                            (AtomType.Atom ([]: string list)))


        module Sample =
            let rec mounted =
                Atom.create (RootAtomPath (FsBeacon.storeRoot, AtomName (nameof mounted))) (AtomType.Atom false)


            module Operation =
                [<RequireQualifiedAccess>]
                type Operation<'T> =
                    | Waiting
                    | Ok of 'T
                    | Progress of int
                    | Error of exn

                let asyncOperationAtom fn =
                    let operationAtom = Atom.Primitives.atom Operation.Waiting

                    let asyncWriteAtom =
                        Atom.Primitives.asyncSelector
                            (fun _ -> promise { return () })
                            (fun _ setter _ ->
                                promise {
                                    Atom.set setter operationAtom (Operation.Progress 0)

                                    try
                                        let! result = fn ()
                                        Atom.set setter operationAtom (Operation.Ok result)
                                    with
                                    | ex -> Atom.set setter operationAtom (Operation.Error ex)
                                })

                    Atom.Primitives.selector
                        (fun getter -> Atom.get getter operationAtom)
                        (fun _ setter _ -> Atom.set setter asyncWriteAtom ())

                let createFileDownloadAtom url =
                    asyncOperationAtom
                        (fun () ->
                            promise {
                                do! Promise.sleep 1000
                                return $"downloaded after 1s. url: {url}"
                            })

            let rec upvote = Atoms.Join.createJoinAtom (nameof upvote)
            let rec downvote = Atoms.Join.createJoinAtom (nameof downvote)


    module Selectors =
        module Sample =
            let fileIdAtoms =
                Engine.subscribeCollection FsStore.storeRoot Atoms.File.collection (Engine.parseGuidKey FileId)


    module Actions =
        let enableGunSync =
            Atom.Primitives.setSelector
                (fun _getter setter () ->
                    Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | Actions.enableGunSync") getLocals

                    Atom.set
                        setter
                        Atoms.gunOptions
                        (GunOptions.Sync [|
                            GunPeer "https://localhost:49221/gun"
                         |]))

        let enableHubSync =
            Atom.Primitives.setSelector
                (fun _getter setter () ->
                    Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | Actions.enableHubSync") getLocals

                    Atom.set setter Atoms.hubUrl (Some "https://localhost:49211"))

        let disableSync =
            Atom.Primitives.setSelector
                (fun _getter setter () ->
                    Profiling.addTimestamp (fun () -> $"{nameof FsBeacon} | Actions.disableSync") getLocals

                    Atom.set setter Atoms.gunOptions GunOptions.Minimal
                    Atom.set setter Atoms.hubUrl None)

        let signIn =
            Atom.Primitives.setSelector
                (fun _getter setter () ->
                    promise {
                        Profiling.addTimestamp
                            (fun () -> $"{nameof FsBeacon} | SignInContainer [ render ] starting sign up...")
                            getLocals

                        let credentials = $"alias@{Dom.deviceTag}"
                        //
//                            match! signIn (credentials, credentials) with
//                            | Ok _ -> ()
//                            | Error error ->
//                                toast (fun x -> x.description <- $"1: {error}")
//
//                                match! signUp (credentials, credentials) with
//                                | Ok _ -> ()
//                                | Error error -> toast (fun x -> x.description <- $"2: {error}")

                        Atom.set setter Auth.Actions.signUp (credentials, credentials)
                    }
                    |> Promise.start)

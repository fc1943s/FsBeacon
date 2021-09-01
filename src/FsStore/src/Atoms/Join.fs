namespace FsStore.State.Atoms

open System
open FsCore.BaseModel
open FsStore
open FsStore.Model
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs
open FsStore.Bindings


module rec Join =
    let collection = Collection (nameof Join)


    let inline createJoinAtom name =
        let rec internalAtomFamily =
            Atom.Primitives.atomFamily
                (fun (atomPathGuidHash: Guid) ->
                    Engine.createRegisteredAtomWithSubscription
                        (IndexedAtomPath (
                            FsStore.storeRoot,
                            collection,
                            [
                                Gun.AtomKeyFragment (string atomPathGuidHash)
                            ],
                            AtomName name
                        ))
                        (None: string option))

        Atom.Primitives.atomFamily
            (fun (storeAtomPath: StoreAtomPath) ->
                let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

                let guidHash = Crypto.getTextGuidHash (atomPath |> AtomPath.Value)
                let atom = internalAtomFamily guidHash

                Logger.logTrace (fun () -> $"Atoms.Join.joinAtom atomPath={atomPath} guidHash={guidHash}")

                Atom.Primitives.selector
                    (fun getter ->
                        let value = Atom.get getter atom
                        Profiling.addCount (fun () -> $"{nameof FsStore} | {atomPath} joinAtom get")

                        Logger.logTrace
                            (fun () ->
                                $"Atoms.Join.joinAtom get() atomPath={atomPath} guidHash={guidHash} value={value}")

                        value
                        |> Option.map
                            (fun value ->
                                match Json.decode<AtomPath * string option> value with
                                | _, Some value -> value
                                | _ -> null))
                    (fun _ setter newValue ->
                        Profiling.addCount (fun () -> $"{nameof FsStore} | {atomPath} joinAtom set")

                        let newValueJson =
                            match Json.encode (atomPath, newValue) with
                            | String.Valid json -> Some json
                            | _ -> None

                        Logger.logTrace
                            (fun () ->
                                $"Atoms.Join.joinAtom set() atomPath={atomPath} guidHash={guidHash} newValue={newValue}  newValueJson={newValueJson} ")

                        Atom.set setter atom newValueJson))


    let rec tempValue = createJoinAtom (nameof tempValue)

namespace FsStore.State.Atoms

open FsStore.Store
open System
open FsCore.BaseModel
open FsStore
open FsStore.Model
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs
open FsStore.Bindings
open FsStore.Bindings.Jotai


module rec Join =
    let collection = Collection (nameof Join)

    let inline createJoinAtom name =
        let rec internalAtomFamily =
            Store.atomFamilyWithSync
                FsStore.root
                collection
                name
                (fun (_atomPathGuidHash: Guid) -> null: string)
                (fun (atomPathGuidHash: Guid) ->
                    [
                        string atomPathGuidHash
                    ])

        jotaiUtils.atomFamily
            (fun (AtomPath atomPath) ->

                let guidHash = Crypto.getTextGuidHash atomPath
                let atom = internalAtomFamily guidHash

                Logger.logTrace (fun () -> $"tempValueWrapper constructor. atomPath={atomPath} guidHash={guidHash}")

                let wrapper =
                    Primitives.rawSelector
                        (fun getter ->
                            let value = Store.value getter atom
                            Profiling.addCount $"{atomPath} tempValue set"

                            Logger.logTrace
                                (fun () ->
                                    $"tempValueWrapper.get(). atomPath={atomPath} guidHash={guidHash} value={value}")

                            match value with
                            | null -> null
                            | _ ->
                                match Json.decode<string * string option> value with
                                | _, Some value -> value
                                | _ -> null)
                        (fun _ setter newValue ->
                            Profiling.addCount $"{atomPath} tempValue set"

                            Logger.logTrace
                                (fun () ->
                                    $"tempValueWrapper.set(). atomPath={atomPath} guidHash={guidHash} newValue={newValue}")

                            let newValue = Json.encode (atomPath, newValue |> Option.ofObj)

                            Logger.logTrace (fun () -> $"tempValueWrapper.set(). newValue2={newValue} ")

                            Store.set setter atom (newValue |> box |> unbox))

                wrapper)
            Object.compare


    let rec tempValue = createJoinAtom (nameof tempValue)

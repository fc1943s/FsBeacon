namespace FsStore.Bindings

open Fable.Core.JsInterop
open Fable.Core
open Fable.React


module Jotai =
    //    type AtomConfig<'A> =
//        abstract member toString : unit -> string
//        abstract member onMount : (('TValue -> unit) -> unit -> unit) with get, set

    type AtomConfig<'A> =
        {
            [<Emit "toString">]
            ToString: unit -> string

            [<Emit "init">]
            init: 'A option

            [<Emit "read">]
            Read: Read<'A>

            [<Emit "write">]
            Write: Write<'A>
        }

    and Getter<'A> = AtomConfig<'A> -> 'A
    and Setter<'A> = AtomConfig<'A> -> 'A -> unit

    and [<RequireQualifiedAccess>] AtomType<'A> =
        | Atom of defaultValue: 'A
        | ReadSelector of read: Read<'A>
        | Selector of read: Read<'A> * write: Write<'A>
        | WriteOnlyAtom of write: Write<'A>

    and Read<'A> = Getter<obj> -> 'A
    and Write<'A> = Getter<obj> -> Setter<obj> -> 'A -> unit

    type AsyncRead<'A> = Getter<obj> -> JS.Promise<'A>
    type AsyncWrite<'A> = Getter<obj> -> Setter<obj> -> 'A -> JS.Promise<unit>

    type CompareFn<'TValue> = 'TValue -> 'TValue -> bool

    type IJotai =
        abstract Provider : obj -> obj
        abstract atom : 'A -> AtomConfig<'A>
        abstract atom : Read<'A> * Write<'A> option -> AtomConfig<'A>
        abstract atom : AsyncRead<'A> * AsyncWrite<'A> option -> AtomConfig<'A>
        abstract useAtom : AtomConfig<'A> -> 'A * ('A -> unit)

    let jotai: IJotai = importAll "jotai"

    type IJotaiUtils =
        abstract atomFamily : ('TKey -> AtomConfig<'A>) -> CompareFn<'A> -> ('TKey -> AtomConfig<'A>)
        abstract atomWithDefault : Read<'A> -> AtomConfig<'A>
        abstract atomWithReducer : 'A -> ('A -> 'A -> 'A) -> AtomConfig<'A>
        abstract atomWithStorage : string -> 'A -> AtomConfig<'A>
        abstract selectAtom : AtomConfig<'A> -> ('A -> 'U) -> CompareFn<'A> -> AtomConfig<'U>
        abstract splitAtom : AtomConfig<'A []> -> AtomConfig<AtomConfig<'A> []>
        abstract useAtomValue : AtomConfig<'A> -> 'A
        //        abstract useHydrateAtoms : (AtomConfig<'A> * 'TValue) [] -> AtomScope -> unit
        abstract useHydrateAtoms : (AtomConfig<obj> * obj) [] -> unit
        abstract useUpdateAtom : AtomConfig<'A> -> ('A -> unit)

        abstract useAtomCallback : (Getter<'A> * Setter<'A> * 'TArg -> JS.Promise<'A>) -> ('TArg -> JS.Promise<'A>)

        abstract waitForAll : AtomConfig<'T> [] -> AtomConfig<'T []>


    let jotaiUtils: IJotaiUtils = importAll "jotai/utils"


[<AutoOpen>]
module JotaiMagic =

    type Jotai.IJotai with
        member inline _.provider children =
            ReactBindings.React.createElement (Jotai.jotai.Provider, (), children)

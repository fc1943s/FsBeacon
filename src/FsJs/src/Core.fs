namespace FsJs

open System
open Fable.DateFunctions
open Fable.Core.JsInterop


module Char =
    let inline getNumericValue (char: char) =
        (((string char).ToLower ())?charCodeAt 0) - 97 + 1


module DateTime =
    let inline format format (dateTime: DateTime) = dateTime.Format format

    let inline addDays (days: int) (dateTime: DateTime) = dateTime.AddDays days

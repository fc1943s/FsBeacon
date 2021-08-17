namespace FsStore

open FsStore.Model
open Microsoft.FSharp.Core.Operators
open FsJs


module Atoms =
    let rec logLevel = Store.atomWithStorage FsStore.root (nameof logLevel) Dom.DEFAULT_LOG_LEVEL
    let rec showDebug = Store.atomWithStorage FsStore.root (nameof showDebug) Dom.deviceInfo.IsTesting
    let rec gunOptions = Store.atomWithStorage FsStore.root (nameof gunOptions) (GunOptions.Sync [||])
    let rec hubUrl = Store.atomWithStorage FsStore.root (nameof hubUrl) (None: string option)
    let rec gunTrigger = Store.atom FsStore.root (nameof gunTrigger) 0
    let rec hubTrigger = Store.atom FsStore.root (nameof hubTrigger) 0
    let rec routeTrigger = Store.atom FsStore.root (nameof routeTrigger) 0
    let rec sessionRestored = Store.atom FsStore.root (nameof sessionRestored) false

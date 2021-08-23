namespace FsUi.State.Atoms

open FsStore
open FsUi.Model
open FsUi
open FsStore.Model


module rec Ui =

    let rec darkMode =
        Atom.createRegisteredWithStorage
            (RootAtomPath (FsUi.storeRoot, AtomName (nameof darkMode)))
            UiState.Default.DarkMode
        |> Atom.enableAdapters

    let rec fontSize =
        Atom.createRegisteredWithStorage
            (RootAtomPath (FsUi.storeRoot, AtomName (nameof fontSize)))
            UiState.Default.FontSize
        |> Atom.enableAdapters

    let rec systemUiFont =
        Atom.createRegisteredWithStorage
            (RootAtomPath (FsUi.storeRoot, AtomName (nameof systemUiFont)))
            UiState.Default.SystemUiFont
        |> Atom.enableAdapters

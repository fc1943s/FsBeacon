namespace FsUi.State.Selectors

open FsStore
open FsUi
open FsUi.Model
open FsUi.State


module rec Ui =
    let rec uiState =
        Atom.Primitives.readSelector
            (fun getter ->
                {
                    DarkMode = Atom.get getter Atoms.Ui.darkMode
                    FontSize = Atom.get getter Atoms.Ui.fontSize
                    SystemUiFont = Atom.get getter Atoms.Ui.systemUiFont
                })

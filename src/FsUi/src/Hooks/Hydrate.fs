namespace FsUi.Hooks

open FsStore
open FsUi.Model
open FsUi.State


module Hydrate =
    let inline hydrateUiState _ setter (uiState: UiState) =
        promise {
            let set atom value = Store.set setter atom value

            set Atoms.Ui.darkMode uiState.DarkMode
            set Atoms.Ui.fontSize uiState.FontSize
            set Atoms.Ui.systemUiFont uiState.SystemUiFont
        }

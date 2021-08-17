namespace FsUi.Components

open Feliz
open FsStore.Hooks
open FsStore
open FsStore.Model
open FsUi.Bindings

module CheckboxInput =
    [<ReactComponent>]
    let CheckboxInput
        (input: {| Atom: Atom<_>
                   Label: string option
                   Props: Ui.IChakraProps -> unit |})
        =
        let value, setValue = Store.useState input.Atom

        Checkbox.Checkbox
            input.Label
            (fun x ->
                x.isChecked <- value
                x.onChange <- fun _ -> promise { setValue (not value) }
                input.Props x)

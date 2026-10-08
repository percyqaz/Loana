namespace Loana.Desktop.Browser

open System.Collections.Generic
open Loana.Data

type ErrorsTab =
    {
        mutable Items: IReadOnlyList<WordlistError>
        mutable Position: int
    }

    member this.Selected: WordlistError option =
        if this.Position >= 0 && this.Position < this.Items.Count then Some this.Items.[this.Position] else None

    member this.Refresh(words: WordBank) : unit =
        let current_selected = this.Selected
        this.Items <- words.Errors

        this.Position <-
            match current_selected with
            | Some error -> Seq.tryFindIndex ((=) error) this.Items |> Option.defaultValue 0
            | None -> 0

    static member Create(words: WordBank) : ErrorsTab =
        { Items = words.Errors; Position = max 0 (words.Errors.Count - 1) }

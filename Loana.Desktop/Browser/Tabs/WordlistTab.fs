namespace Loana.Desktop.Browser

open System.Collections.Generic
open Loana.Data

type WordlistTab =
    {
        Wordlist: string
        mutable Items: IReadOnlyList<WordlistEntry>
        mutable Position: int
    }

    member this.Selected: WordlistEntry option =
        if this.Position < this.Items.Count then Some this.Items.[this.Position] else None

    member this.Refresh(words: WordBank) : unit =
        let current_selected = this.Selected

        this.Items <- words.Entries |> Seq.where(fun x -> x.Source.WordlistName = this.Wordlist) |> ResizeArray

        this.Position <-
            match current_selected with
            | Some i -> Seq.tryFindIndex ((=) i) this.Items |> Option.defaultValue 0
            | None -> 0

    static member Create(wordlist: string, words: WordBank) : WordlistTab =
        let tab = { Wordlist = wordlist; Items = []; Position = 0 }
        tab.Refresh(words)
        tab

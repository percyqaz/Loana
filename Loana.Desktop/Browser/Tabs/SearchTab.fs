namespace Loana.Desktop.Browser

open System
open System.Collections.Generic
open Loana.Data
open Loana.Desktop.CLI

type SearchTab =
    {
        mutable SearchFocused: bool
        Buffer: TextBuffer
        mutable Items: IReadOnlyList<WordlistEntry>
        mutable Position: int
    }

    member this.Query = this.Buffer.ToString()

    member this.Refresh(words: WordBank) : unit =
        let current_item =
            if this.Position < this.Items.Count then Some this.Items.[this.Position] else None

        this.Items <-
            words.Entries
            |> Seq.where(fun x ->
                let t =
                    match x.Item with
                    | WordlistItem.Noun n -> n.Translation
                    | WordlistItem.Verb v -> v.Infinitive
                    | WordlistItem.Vocab v -> v

                t.Deutsch.Contains(this.Query, StringComparison.InvariantCultureIgnoreCase)
                || t.EnglishAsciiIdentifier.Contains(this.Query, StringComparison.InvariantCultureIgnoreCase)
            )
            |> ResizeArray

        this.Position <-
            match current_item with
            | Some i -> Seq.tryFindIndex ((=) i) this.Items |> Option.defaultValue 0
            | None -> 0

    static member Create(words: WordBank) : SearchTab =
        let tab =
            {
                SearchFocused = true
                Buffer = TextBuffer()
                Items = []
                Position = 0
            }

        tab.Refresh(words)
        tab

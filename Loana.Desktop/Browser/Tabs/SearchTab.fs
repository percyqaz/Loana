namespace Loana.Desktop.Browser

open System
open System.Collections.Generic
open Loana.Data
open Loana.Desktop.CLI

type SearchTab =
    {
        mutable SearchFocused: bool
        Buffer: TextBuffer
        mutable Results: IReadOnlyList<WordlistEntry>
        mutable Position: int
    }

    member this.Query = this.Buffer.ToString()

    member this.UpdateSearchResults(words: WordBank) : unit =
        let current_item =
            if this.Position < this.Results.Count then Some this.Results.[this.Position] else None

        this.Results <-
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
            | Some i -> Seq.tryFindIndex ((=) i) this.Results |> Option.defaultValue 0
            | None -> 0

    static member Create(words: WordBank) : SearchTab =
        let tab =
            {
                SearchFocused = true
                Buffer = TextBuffer()
                Results = []
                Position = 0
            }

        tab.UpdateSearchResults(words)
        tab

namespace Loana.Desktop.Browser

open System.Collections.Generic
open System.Drawing
open Loana.Data
open Loana.Desktop.CLI

[<RequireQualifiedAccess>]
type WordlistGroupsSelection =
    | Wordlist of group: string * list: string
    | WordlistGroup of string

    member this.HighlightString() : string =
        match this with
        | Wordlist(_, list) -> $"| {list}".ForeColor(Color.LightGreen)
        | WordlistGroup(group) -> $"@ {group}".ForeColor(Color.LightBlue).Bold()

    override this.ToString() : string =
        match this with
        | Wordlist(_, list) -> $"| {list}"
        | WordlistGroup(group) -> $"@ {group}"

type WordlistGroupsTab =
    {
        mutable Items: IReadOnlyList<WordlistGroupsSelection>
        mutable Position: int
    }

    member this.Selection = this.Items.[this.Position]

    member this.Refresh(words: WordBank) : unit =
        let current_item =
            if this.Position < this.Items.Count then Some this.Selection else None

        let available =
            seq {
                for group in words.Groups do
                    yield WordlistGroupsSelection.WordlistGroup(group.Name)

                    for list in group.WordlistNames do
                        yield WordlistGroupsSelection.Wordlist(group.Name, list)
            }

        this.Items <- ResizeArray(available)

        this.Position <-
            match current_item with
            | Some item ->
                match Seq.tryFindIndex ((=) item) this.Items with
                | None -> 0
                | Some index -> index
            | None -> 0

    // todo: start on a particular selection
    static member Create(words: WordBank) : WordlistGroupsTab =
        let tab = { Items = []; Position = 0 }
        tab.Refresh(words)
        tab

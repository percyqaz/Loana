namespace Loana.Desktop.Browser

open Loana.Data

[<RequireQualifiedAccess>]
type WordlistGroupsSelection =
    | Wordlist of group: string * list: string
    | WordlistGroup of string

type WordlistGroupsTab =
    {
        mutable Selection: WordlistGroupsSelection
    }

    // refresh: ensure selection exists still, otherwise reset

    static member Create(words: WordBank) : WordlistGroupsTab =
        { Selection = WordlistGroupsSelection.WordlistGroup(words.Groups.[0].Name) }

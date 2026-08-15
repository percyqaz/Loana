namespace Loana.Desktop.Browser

open Loana.Data

type ErrorsTab =
    {
        mutable Position: int
    }

    // refresh: ensure position isn't out of range, if so set to 0

    static member Create(words: WordBank) : ErrorsTab = { Position = words.Errors.Count - 1 }

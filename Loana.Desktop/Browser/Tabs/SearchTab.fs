namespace Loana.Desktop.Browser

open System.Collections.Generic
open Loana.Data

type SearchTab =
    {
        mutable SearchFocused: bool
        mutable Query: string
        mutable Results: IReadOnlyList<WordlistEntry>
        mutable Position: int
    }

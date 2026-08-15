namespace Loana.Desktop.Browser

open System.Collections.Generic
open Loana.Data

type WordlistTab = { Wordlist: string; mutable Items: IReadOnlyList<WordlistEntry>; mutable Position: int }

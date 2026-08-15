namespace Loana.Desktop.Browser

open System.Runtime.CompilerServices

type SearchTabCommands =

    [<Extension>]
    static member Up(tab: SearchTab) : unit =
        if tab.Results.Count > 0 then
            tab.Position <- if tab.Position = 0 then tab.Results.Count - 1 else tab.Position - 1

    [<Extension>]
    static member Down(tab: SearchTab) : unit =
        if tab.Results.Count > 0 then
            tab.Position <- if tab.Position + 1 = tab.Results.Count then 0 else tab.Position + 1

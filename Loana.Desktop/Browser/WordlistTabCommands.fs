namespace Loana.Desktop.Browser

open System.Runtime.CompilerServices

type WordlistTabCommands =

    [<Extension>]
    static member Up(tab: WordlistTab) : unit =
        if tab.Items.Count > 0 then
            tab.Position <- if tab.Position = 0 then tab.Items.Count - 1 else tab.Position - 1

    [<Extension>]
    static member Down(tab: WordlistTab) : unit =
        if tab.Items.Count > 0 then
            tab.Position <- if tab.Position + 1 = tab.Items.Count then 0 else tab.Position + 1

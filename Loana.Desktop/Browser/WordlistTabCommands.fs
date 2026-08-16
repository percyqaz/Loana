namespace Loana.Desktop.Browser

open System.Runtime.CompilerServices
open Loana.Data

type WordlistTabCommands =

    [<Extension>]
    static member Up(tab: WordlistTab) : unit =
        if tab.Items.Count > 0 then
            tab.Position <- if tab.Position = 0 then tab.Items.Count - 1 else tab.Position - 1

    [<Extension>]
    static member Down(tab: WordlistTab) : unit =
        if tab.Items.Count > 0 then
            tab.Position <- if tab.Position + 1 = tab.Items.Count then 0 else tab.Position + 1

    [<Extension>]
    static member MoveUp(tab: WordlistTab, words: WordBank) : unit =
        if tab.Position > 0 && tab.Position < tab.Items.Count then
            let to_move = tab.Items.[tab.Position - 1]
            let reference_point = tab.Items.[tab.Position]
            words.MoveAfter(to_move, reference_point)

    [<Extension>]
    static member MoveDown(tab: WordlistTab, words: WordBank) : unit =
        if tab.Position + 1 < tab.Items.Count then
            let to_move = tab.Items.[tab.Position]
            let reference_point = tab.Items.[tab.Position + 1]
            words.MoveAfter(to_move, reference_point)

namespace Loana.Desktop.Browser

open Loana.Data
open Loana.Desktop.CLI

type LeftTab =
    | Wordlists of WordlistGroupsTab
    | Wordlist of WordlistTab

type RightTab =
    | RWordlists of WordlistGroupsTab
    | RWordlist of WordlistTab
    | Search of SearchTab
    | Errors of ErrorsTab

type BrowserState =
    {
        mutable Running: bool
        UIContext: UIContext
        Words: WordBank
        mutable Left: LeftTab
        mutable Right: RightTab
    }

    static member Create(words: WordBank, ui_ctx: UIContext) : BrowserState =
        {
            Running = true
            UIContext = ui_ctx
            Words = words
            Left = Wordlists(WordlistGroupsTab.Create(words))
            Right = RWordlists(WordlistGroupsTab.Create(words))
        }

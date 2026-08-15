namespace Loana.Desktop.Browser

open System
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
        mutable LeftTab: LeftTab
        mutable RightTab: RightTab
        mutable RightFocused: bool
    }

    static member Create(words: WordBank, ui_ctx: UIContext) : BrowserState =
        {
            Running = true
            UIContext = ui_ctx
            Words = words
            LeftTab = Wordlists(WordlistGroupsTab.Create(words))
            RightTab = Search(SearchTab.Create(words))
            RightFocused = false
        }

    member this.AddKey(key: ConsoleKeyInfo) : unit =
        match this.RightTab with
        | Search tab when this.RightFocused && tab.SearchFocused ->
            if tab.Buffer.TryAddKey(key) then tab.UpdateSearchResults(this.Words) else tab.SearchFocused <- false
        | _ -> this.UIContext.Buffer.AddKey(key)

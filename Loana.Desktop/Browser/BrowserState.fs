namespace Loana.Desktop.Browser

open System
open Loana.Data
open Loana.Desktop.CLI

type Tab =
    | Wordlists of WordlistGroupsTab
    | Wordlist of WordlistTab

type PopupTab =
    | Search of SearchTab
    | Errors of ErrorsTab
    | NoPopup

type BrowserState =
    {
        mutable Running: bool
        UIContext: UIContext
        Words: WordBank
        mutable LeftTab: Tab
        mutable RightTab: Tab
        mutable RightPopup: PopupTab
        mutable RightFocused: bool
    }

    static member Create(words: WordBank, ui_ctx: UIContext) : BrowserState =
        {
            Running = true
            UIContext = ui_ctx
            Words = words
            LeftTab = Wordlists(WordlistGroupsTab.Create(words))
            RightTab = Wordlists(WordlistGroupsTab.Create(words))
            RightPopup = NoPopup
            RightFocused = false
        }

    member this.AddKey(key: ConsoleKeyInfo) : unit =
        match this.RightPopup with
        | Search tab when this.RightFocused && tab.SearchFocused ->
            if tab.Buffer.TryAddKey(key) then tab.Refresh(this.Words) else tab.SearchFocused <- false
        | _ -> this.UIContext.Buffer.AddKey(key)

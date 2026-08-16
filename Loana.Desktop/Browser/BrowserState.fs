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
        Data: LoanaState
        mutable LeftTab: Tab
        mutable RightTab: Tab
        mutable RightPopup: PopupTab
        mutable RightFocused: bool
    }

    member this.Words = this.Data.Words

    static member Create(data: LoanaState, ui_ctx: UIContext) : BrowserState =
        {
            Running = true
            UIContext = ui_ctx
            Data = data
            LeftTab = Wordlists(WordlistGroupsTab.Create(data.Words))
            RightTab = Wordlists(WordlistGroupsTab.Create(data.Words))
            RightPopup = NoPopup
            RightFocused = false
        }

    member this.AddKey(key: ConsoleKeyInfo) : unit =
        match this.RightPopup with
        | Search tab when this.RightFocused && tab.SearchFocused ->
            if tab.Buffer.TryAddKey(key) then tab.Refresh(this.Words) else tab.SearchFocused <- false
        | _ -> this.UIContext.Buffer.AddKey(key)

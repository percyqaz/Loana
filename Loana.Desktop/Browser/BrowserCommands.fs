namespace Loana.Desktop.Browser

open System
open System.Runtime.CompilerServices
open Loana.Data
open Loana.Desktop.CLI

type BrowserCommands =

    [<Extension>]
    static member Exit(state: BrowserState) : unit =
        if state.RightFocused then
            match state.RightPopup with
            | Search _
            | Errors _ -> state.RightPopup <- NoPopup
            | NoPopup ->
                match state.RightTab with
                | Wordlist _ -> state.RightTab <- Wordlists(WordlistGroupsTab.Create(state.Words))
                | Wordlists _ -> state.Running <- false
        else
            match state.LeftTab with
            | Wordlist _ -> state.LeftTab <- Wordlists(WordlistGroupsTab.Create(state.Words))
            | Wordlists _ -> state.Running <- false

    [<Extension>]
    static member Echo(state: BrowserState, text: string) : unit = state.UIContext.StatusLine <- text

    [<Extension>]
    static member Up(state: BrowserState) : unit =
        if state.RightFocused then
            match state.RightPopup with
            | Search tab -> tab.Up()
            | Errors tab -> tab.Up()
            | NoPopup ->
                match state.RightTab with
                | Wordlist tab -> tab.Up()
                | Wordlists tab -> tab.Up()
        else
            match state.LeftTab with
            | Wordlist tab -> tab.Up()
            | Wordlists tab -> tab.Up()

    [<Extension>]
    static member Down(state: BrowserState) : unit =
        if state.RightFocused then
            match state.RightPopup with
            | Search tab -> tab.Down()
            | Errors tab -> tab.Down()
            | NoPopup ->
                match state.RightTab with
                | Wordlist tab -> tab.Down()
                | Wordlists tab -> tab.Down()
        else
            match state.LeftTab with
            | Wordlist tab -> tab.Down()
            | Wordlists tab -> tab.Down()

    [<Extension>]
    static member Left(state: BrowserState) : unit = state.RightFocused <- false

    [<Extension>]
    static member Right(state: BrowserState) : unit = state.RightFocused <- true

    [<Extension>]
    static member MoveUp(state: BrowserState) : unit =
        if state.RightFocused then
            match state.RightPopup with
            | Search tab -> tab.MoveUp(state.Words)
            | Errors _ -> ()
            | NoPopup ->
                match state.RightTab with
                | Wordlist tab -> tab.MoveUp(state.Words)
                | Wordlists _ -> state.UIContext.StatusLine <- "Not supported"
        else
            match state.LeftTab with
            | Wordlist tab -> tab.MoveUp(state.Words)
            | Wordlists _ -> state.UIContext.StatusLine <- "Not supported"

        state.Refresh()

    [<Extension>]
    static member MoveDown(state: BrowserState) : unit =
        if state.RightFocused then
            match state.RightPopup with
            | Search tab -> tab.MoveDown(state.Words)
            | Errors _ -> ()
            | NoPopup ->
                match state.RightTab with
                | Wordlist tab -> tab.MoveDown(state.Words)
                | Wordlists _ -> state.UIContext.StatusLine <- "Not supported"
        else
            match state.LeftTab with
            | Wordlist tab -> tab.MoveDown(state.Words)
            | Wordlists _ -> state.UIContext.StatusLine <- "Not supported"

        state.Refresh()

    [<Extension>]
    static member MoveLeft(state: BrowserState) : unit =
        let last_in_wordlist (selection: WordlistGroupsSelection option) =
            match selection with
            | Some(WordlistGroupsSelection.Wordlist(_, wordlist)) ->
                state.Words.Entries |> Seq.where(fun x -> x.Source.WordlistName = wordlist) |> Seq.tryLast
            | _ -> None

        let left_target, left_type =
            match state.LeftTab with
            | Wordlist tab -> tab.Selected, "Wordlist"
            | Wordlists tab -> last_in_wordlist(tab.Selected), "Wordlists"

        let right_target, right_type =
            match state.RightPopup with
            | Search tab -> tab.Selected, "Search"
            | Errors _
            | NoPopup ->
                match state.RightTab with
                | Wordlist tab -> tab.Selected, "Wordlist"
                | Wordlists tab -> last_in_wordlist(tab.Selected), "Wordlists"

        match left_target, right_target with
        | Some left, Some right when not(left_type = "Wordlists" && right_type = "Wordlists") ->
            state.Words.MoveAfter(right, left)

            if state.RightFocused && right_type = "Wordlist" then
                state.Up()

            state.Refresh()
        | _ -> state.UIContext.StatusLine <- "Not supported"

    [<Extension>]
    static member MoveRight(state: BrowserState) : unit =
        let last_in_wordlist (selection: WordlistGroupsSelection option) =
            match selection with
            | Some(WordlistGroupsSelection.Wordlist(_, wordlist)) ->
                state.Words.Entries |> Seq.where(fun x -> x.Source.WordlistName = wordlist) |> Seq.tryLast
            | _ -> None

        let left_target, left_type =
            match state.LeftTab with
            | Wordlist tab -> tab.Selected, "Wordlist"
            | Wordlists tab -> last_in_wordlist(tab.Selected), "Wordlists"

        let right_target, right_type =
            match state.RightPopup with
            | Search tab -> tab.Selected, "Search"
            | Errors _
            | NoPopup ->
                match state.RightTab with
                | Wordlist tab -> tab.Selected, "Wordlist"
                | Wordlists tab -> last_in_wordlist(tab.Selected), "Wordlists"

        match left_target, right_target with
        | Some left, Some right when not(left_type = "Wordlists" && right_type = "Wordlists") ->
            state.Words.MoveAfter(left, right)

            if not state.RightFocused && left_type = "Wordlist" then
                state.Up()

            state.Refresh()
        | _ -> state.UIContext.StatusLine <- "Not supported"

    [<Extension>]
    static member Refresh(state: BrowserState) : unit =
        match state.RightPopup with
        | Search tab -> tab.Refresh(state.Words)
        | Errors tab -> tab.Refresh(state.Words)
        | NoPopup -> ()

        match state.RightTab with
        | Wordlists tab -> tab.Refresh(state.Words)
        | Wordlist tab -> tab.Refresh(state.Words)

        match state.LeftTab with
        | Wordlists tab -> tab.Refresh(state.Words)
        | Wordlist tab -> tab.Refresh(state.Words)

    [<Extension>]
    static member Select(state: BrowserState) : unit =
        let inline display_error (error: WordlistError) : unit =
            Console.Clear()
            Console.WriteLine(error.Reason.ForeColor(0xFF8888))
            Console.ReadKey(true) |> ignore

        if state.RightFocused then
            match state.RightPopup with
            | Search _ -> state.UIContext.StatusLine <- "Not supported"
            | Errors tab -> tab.Selected |> Option.iter(display_error)
            | NoPopup ->
                match state.RightTab with
                | Wordlist _ -> state.UIContext.StatusLine <- "Not supported"
                | Wordlists tab ->
                    match tab.Selected with
                    | Some(WordlistGroupsSelection.Wordlist(_, wordlist)) ->
                        state.RightTab <- Wordlist(WordlistTab.Create(wordlist, state.Words))
                    | _ -> ()
        else
            match state.LeftTab with
            | Wordlist _ -> state.UIContext.StatusLine <- "Not supported"
            | Wordlists tab ->
                match tab.Selected with
                | Some(WordlistGroupsSelection.Wordlist(_, wordlist)) ->
                    state.LeftTab <- Wordlist(WordlistTab.Create(wordlist, state.Words))
                | _ -> ()

        state.Refresh()

    [<Extension>]
    static member Search(state: BrowserState) : unit =
        state.RightFocused <- true

        match state.RightPopup with
        | Search tab -> tab.SearchFocused <- true
        | _ -> state.RightPopup <- Search(SearchTab.Create(state.Words))

    [<Extension>]
    static member Errors(state: BrowserState) : unit =
        state.RightFocused <- true
        state.RightPopup <- Errors(ErrorsTab.Create(state.Words))

    [<Extension>]
    static member Save(state: BrowserState) : unit =
        state.Words.WriteToDirectory(state.Data.Path)
        state.UIContext.StatusLine <- sprintf "Saved (%s)" (DateTime.Now.ToShortTimeString())

    [<Extension>]
    static member Delete(state: BrowserState) : unit =
        if state.RightFocused then
            match state.RightPopup with
            | Errors tab ->
                match tab.Selected with
                | Some error ->
                    state.Words.RemoveError(error)
                    state.Refresh()
                | None -> ()
            | _ -> state.UIContext.StatusLine <- "Not supported"
        else
            state.UIContext.StatusLine <- "Not supported"

    [<Extension>]
    static member DispatchCommand(state: BrowserState, command: string) : unit =
        let split =
            command.Split(' ', 2, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

        let command, args = split.[0], if split.Length > 1 then split.[1] else ""

        match command with
        | "exit"
        | "q" -> state.Exit()
        | "echo" -> state.Echo(args)
        | "up" -> state.Up()
        | "down" -> state.Down()
        | "left" -> state.Left()
        | "right" -> state.Right()
        | "move_up" -> state.MoveUp()
        | "move_down" -> state.MoveDown()
        | "move_left" -> state.MoveLeft()
        | "move_right" -> state.MoveRight()
        | "search" -> state.Search()
        | "errors" -> state.Errors()
        | "delete" -> state.Delete()
        | "select" -> state.Select()
        | "save" -> state.Save()
        | _ -> state.UIContext.StatusLine <- sprintf "Unrecognised command '%s'" command

    [<Extension>]
    static member DispatchMessage(state: BrowserState, message: string) : unit =
        if message.StartsWith(':') then
            state.DispatchCommand(message.Substring(1))
        else
            state.UIContext.StatusLine <- sprintf "Unrecognised message '%s'" message

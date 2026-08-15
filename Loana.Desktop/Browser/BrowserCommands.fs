namespace Loana.Desktop.Browser

open System
open System.Runtime.CompilerServices

type BrowserCommands =

    [<Extension>]
    static member Exit(state: BrowserState) : unit = state.Running <- false

    [<Extension>]
    static member Echo(state: BrowserState, text: string) : unit = state.UIContext.StatusLine <- text

    [<Extension>]
    static member Up(state: BrowserState) : unit =
        if state.RightFocused then
            match state.RightPopup with
            | Search tab -> tab.Up()
            | Errors tab -> ()
            | NoPopup ->
                match state.RightTab with
                | _ -> ()
        else
            match state.LeftTab with
            | _ -> ()

    [<Extension>]
    static member Down(state: BrowserState) : unit =
        if state.RightFocused then
            match state.RightPopup with
            | Search tab -> tab.Down()
            | Errors tab -> ()
            | NoPopup ->
                match state.RightTab with
                | _ -> ()
        else
            match state.LeftTab with
            | _ -> ()

    [<Extension>]
    static member Left(state: BrowserState) : unit = state.RightFocused <- false

    [<Extension>]
    static member Right(state: BrowserState) : unit = state.RightFocused <- true

    [<Extension>]
    static member Search(state: BrowserState) : unit =
        state.RightFocused <- true

        match state.RightPopup with
        | Search tab -> tab.SearchFocused <- true
        | _ -> state.RightPopup <- Search(SearchTab.Create(state.Words))

    [<Extension>]
    static member DispatchCommand(state: BrowserState, command: string) : unit =
        let split =
            command.Split(' ', 2, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

        let command, args = split.[0], if split.Length > 1 then split.[1] else ""

        match command with
        | "exit" -> state.Exit()
        | "echo" -> state.Echo(args)
        | "up" -> state.Up()
        | "down" -> state.Down()
        | "left" -> state.Left()
        | "right" -> state.Right()
        | "search" -> state.Search()
        | _ -> state.UIContext.StatusLine <- sprintf "Unrecognised command '%s'" command

    [<Extension>]
    static member DispatchMessage(state: BrowserState, message: string) : unit =
        if message.StartsWith(':') then
            state.DispatchCommand(message.Substring(1))
        else
            state.UIContext.StatusLine <- sprintf "Unrecognised message '%s'" message

namespace Loana.Desktop.Study

open System
open System.Runtime.CompilerServices

type StudySessionCommands =

    static let next (state: StudySessionState) : unit =
        match state.Cards.Next() with
        | Some new_card -> state.CardState <- Front new_card
        | None -> state.Running <- false

    [<Extension>]
    static member Exit(state: StudySessionState) : unit = state.Running <- false

    [<Extension>]
    static member Echo(state: StudySessionState, text: string) : unit = state.UIContext.StatusLine <- text

    [<Extension>]
    static member Reveal(state: StudySessionState) : unit =
        match state.CardState with
        | Front card -> state.CardState <- Back card
        | Back _ -> ()

    [<Extension>]
    static member Forgot(state: StudySessionState) : unit =
        match state.CardState with
        | Back card ->
            state.ForgotCount <- state.ForgotCount + 1
            state.Cards.Forgot(card) |> Seq.iter state.LogMessage
            next(state)
        | Front _ -> ()

    [<Extension>]
    static member Bad(state: StudySessionState) : unit =
        match state.CardState with
        | Back card ->
            state.BadCount <- state.BadCount + 1
            state.Cards.Bad(card) |> Seq.iter state.LogMessage
            next(state)
        | Front _ -> ()

    [<Extension>]
    static member Ok(state: StudySessionState) : unit =
        match state.CardState with
        | Back card ->
            state.OkCount <- state.OkCount + 1
            state.Cards.Ok(card) |> Seq.iter state.LogMessage
            next(state)
        | Front _ -> ()

    [<Extension>]
    static member Good(state: StudySessionState) : unit =
        match state.CardState with
        | Back card ->
            state.GoodCount <- state.GoodCount + 1
            state.Cards.Good(card) |> Seq.iter state.LogMessage
            next(state)
        | Front _ -> ()

    [<Extension>]
    static member DispatchCommand(state: StudySessionState, command: string) : unit =
        let split =
            command.Split(' ', 2, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

        let command, args = split.[0], if split.Length > 1 then split.[1] else ""

        match command with
        | "exit" -> state.Exit()
        | "echo" -> state.Echo(args)
        | "reveal" -> state.Reveal()
        | "forgot" -> state.Forgot()
        | "bad" -> state.Bad()
        | "ok" -> state.Ok()
        | "good" -> state.Good()
        | _ -> state.UIContext.StatusLine <- sprintf "Unrecognised command '%s'" command

    [<Extension>]
    static member DispatchMessage(state: StudySessionState, message: string) : unit =
        if message.StartsWith(':') then
            state.DispatchCommand(message.Substring(1))
        else
            state.UIContext.StatusLine <- sprintf "Unrecognised message '%s'" message

namespace Loana.Desktop.Study

open System
open System.Drawing
open Loana.Data
open Loana.Desktop.Vocab
open Loana.Desktop.CLI

type StudySessionResult =
    {
        EndEarly: bool
        Good: int
        Ok: int
        Bad: int
        Forgot: int
    }

    member this.NotGood = this.Ok + this.Bad + this.Forgot

type StudySession(state: StudySessionState) =

    [<Literal>]
    let CARD_AREA = 20

    let draw_card (side: CardSide) : unit =

        let BG_COLOR = 0xFF_101010
        let edges_width = MenuRender.Width - 12

        let inline empty () : unit =
            MenuRender.Write(("".ClearRestOfLine() + "\n").BackColor(BG_COLOR))

        let inline add_margins (line: string) : string =
            let missing_space = MenuRender.Width - edges_width

            if missing_space <= 0 then
                line
            else

            (String.replicate (missing_space / 2) " ").BackColor(BG_COLOR)
            + line
            + (String.replicate ((missing_space + 1) / 2) " ").BackColor(BG_COLOR)

        let inline line (line: string) : unit =
            MenuRender.Write(add_margins(line).ClearRestOfLine() + "\n".BackColor(BG_COLOR))

        empty()
        empty()
        let mutable lines_displayed = 2

        for l in side.Render(edges_width) do
            line(l)
            lines_displayed <- lines_displayed + 1

        for _ = lines_displayed to CARD_AREA do
            empty()

    let draw_log () =
        for l in state.Log do
            MenuRender.WriteLine(l)

    let draw_title () =
        MenuRender.Write(
            $" Loana: {state.Title} ".PadRight(MenuRender.Width - 16),
            Color.White,
            Color.FromArgb(0xFF_303030)
        )

        MenuRender.Write(
            (sprintf " % 2i cards left " (state.Cards.Remaining() + 1)),
            Color.LightGreen,
            Color.FromArgb(0xFF_303030)
        )

        MenuRender.WriteLine()

    member this.Display() : unit =
        match state.CardState with
        | Front current ->
            let front, _ = VocabCard.Render(current)
            draw_title()
            draw_card(front)

            MenuRender.WriteLine(
                MenuRender.Pad(state.UIContext.Buffer.ToString()),
                Color.LightGray,
                Color.FromArgb(0xFF_303030)
            )

            draw_log()
            MenuRender.Redraw()
        | Back current ->
            let _, back = VocabCard.Render(current)
            draw_title()
            draw_card(back)

            MenuRender.WriteLine(
                MenuRender.Pad(state.UIContext.Buffer.ToString()),
                Color.LightGray,
                Color.FromArgb(0xFF_303030)
            )

            draw_log()
            MenuRender.Redraw()


    member this.Run() : StudySessionResult =

        while state.Running do
            this.Display()
            state.UIContext.Buffer.AddKey(Console.ReadKey(true))
            state.UIContext.Buffer.Dispatch(state.DispatchMessage, state.UIContext.StudyKeymap)

        {
            EndEarly = state.Cards.Remaining() > 0
            Good = state.GoodCount
            Ok = state.OkCount
            Bad = state.BadCount
            Forgot = state.ForgotCount
        }

    member this.Log(message: ScheduleResult) : unit = state.LogMessage(message)

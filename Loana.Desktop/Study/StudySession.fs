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

    let draw_card (side: CardSide) =
        let edges_width = MenuRender.Width - 12
        let inner_width = edges_width - 4

        let empty () =
            MenuRender.WriteLine(MenuRender.Pad(""), Color.White, Color.FromArgb(0xFF_101010))

        let horizontal_edge () =
            MenuRender.Write("      ", Color.White, Color.FromArgb(0xFF_101010))
            MenuRender.Write("".PadRight(edges_width), Color.White, Color.FromArgb(0xFF_303030))
            MenuRender.Write("      ", Color.White, Color.FromArgb(0xFF_101010))
            MenuRender.WriteLine()

        let mutable i = 5

        let line (line: CardLine) =
            MenuRender.Write("      ", Color.White, Color.FromArgb(0xFF_101010))
            MenuRender.Write("  ", Color.White, Color.FromArgb(0xFF_303030))

            MenuRender.Write(" ", Color.White, line.BG)
            MenuRender.Write(line.Content)
            MenuRender.Write("".PadLeft(inner_width - 1 - line.Length |> max 0), Color.White, line.BG)

            MenuRender.Write("  ", Color.White, Color.FromArgb(0xFF_303030))
            MenuRender.Write("      ", Color.White, Color.FromArgb(0xFF_101010))
            MenuRender.WriteLine()
            i <- i + 1

        empty()
        empty()
        horizontal_edge()
        side.Lines |> List.iter line
        horizontal_edge()

        for _ = i to CARD_AREA do
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
            let front, back = VocabCard.Render(current)
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
            let front, back = VocabCard.Render(current)
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

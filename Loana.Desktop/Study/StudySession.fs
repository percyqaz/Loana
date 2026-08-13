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

    [<Literal>]
    let LOG_SIZE = 16

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

    member this.Run() : StudySessionResult =
        let mutable end_early = false

        while state.Running && not(end_early) do
            match state.Cards.Next() with
            | None -> state.Running <- false
            | Some current ->

            let front, back = VocabCard.Render(current)

            draw_title()
            draw_card(front)
            MenuRender.WriteLine(MenuRender.Pad("[Space] Reveal"), Color.LightGray, Color.FromArgb(0xFF_303030))
            draw_log()
            MenuRender.Redraw()

            let mutable loop = true

            while loop do
                match Console.ReadKey(true).Key with
                | ConsoleKey.Spacebar -> loop <- false
                | ConsoleKey.Escape ->
                    loop <- false
                    end_early <- true
                | _ -> ()

            if not(end_early) then
                draw_title()
                draw_card(back)
                MenuRender.Write(" [Z] Forgot ", Color.LightGray, Color.FromArgb(0xFF_303030))

                MenuRender.WriteLine(
                    " [,] -1 Level [.] Keep Level [/] +1 Level ".PadLeft(MenuRender.Width - 12),
                    Color.LightGray,
                    Color.FromArgb(0xFF_303030)
                )

                draw_log()
                MenuRender.Redraw()

                let mutable loop = true

                while loop do
                    match Console.ReadKey(true).Key with
                    | ConsoleKey.Escape ->
                        end_early <- true
                        loop <- false
                    | ConsoleKey.Z ->
                        state.Forgot <- state.Forgot + 1
                        state.Cards.Forgot(current) |> Seq.iter this.Log
                        loop <- false
                    | ConsoleKey.OemComma ->
                        state.Bad <- state.Bad + 1
                        state.Cards.Bad(current) |> Seq.iter this.Log
                        loop <- false
                    | ConsoleKey.OemPeriod ->
                        state.Ok <- state.Ok + 1
                        state.Cards.Ok(current) |> Seq.iter this.Log
                        loop <- false
                    | ConsoleKey.Oem2
                    | ConsoleKey.Divide ->
                        state.Good <- state.Good + 1
                        state.Cards.Good(current) |> Seq.iter this.Log
                        loop <- false
                    | _ -> ()

        {
            EndEarly = end_early
            Good = state.Good
            Ok = state.Ok
            Bad = state.Bad
            Forgot = state.Forgot
        }

    member this.Log(message: string) : unit =
        let message =
            message.PadRight(MenuRender.Width).BackColor(Color.FromArgb(0xFF_202020))

        Console.WriteLine(message)
        state.Log.Add(message)

        if state.Log.Count > LOG_SIZE then
            state.Log.RemoveAt(0)

    member this.Log(result: ScheduleResult) : unit = this.Log(result.HighlightString())

namespace Loana.Desktop.Study

open System
open System.Drawing
open Loana.Language
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

[<AbstractClass>]
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
            (sprintf " % 2i cards left " (state.Cards.Count + 1)),
            Color.LightGreen,
            Color.FromArgb(0xFF_303030)
        )

        MenuRender.WriteLine()

    member this.Run() : StudySessionResult =
        let buttons = [| 0; 0; 0; 0 |]
        let mutable end_early = false

        while state.Cards.Count > 0 && not(end_early) do
            let current = state.Cards.[0]
            state.Cards.RemoveAt(0)

            let front, back = this.Render(current)

            draw_title()
            draw_card front
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
                        state.Cards.Clear()
                        end_early <- true
                        loop <- false
                    | ConsoleKey.Z ->
                        buttons.[3] <- buttons.[3] + 1
                        this.Forget(current)
                        loop <- false
                    | ConsoleKey.OemComma ->
                        buttons.[2] <- buttons.[2] + 1
                        this.Demote(current)
                        loop <- false
                    | ConsoleKey.OemPeriod ->
                        buttons.[1] <- buttons.[1] + 1
                        this.Keep(current)
                        loop <- false
                    | ConsoleKey.Oem2
                    | ConsoleKey.Divide ->
                        buttons.[0] <- buttons.[0] + 1
                        this.Promote(current)
                        loop <- false
                    | _ -> ()

        {
            EndEarly = end_early
            Good = buttons.[0]
            Ok = buttons.[1]
            Bad = buttons.[2]
            Forgot = buttons.[3]
        }

    member this.ReplaceNear(card: Card) : unit =
        state.Cards.Insert(min 4 state.Cards.Count, card)

    member this.ReplaceFar(card: Card) : unit = state.Cards.Add(card)

    member this.Log(message: string) : unit =
        Console.WriteLine(message)
        state.Log.Add(message)

        if state.Log.Count > LOG_SIZE then
            state.Log.RemoveAt(0)

    abstract member Forget: Card -> unit
    abstract member Demote: Card -> unit
    abstract member Keep: Card -> unit
    abstract member Promote: Card -> unit
    abstract member Render: Card -> CardSide * CardSide

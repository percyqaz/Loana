namespace Loana.Desktop.CLI

open System
open System.Drawing
open Loana.Language
open Loana.Desktop.CLI

type CardFragment = internal { Text: string; FG: Color; BG: Color }

type CardLine =
    private
        {
            Content: string
            BG: Color
            Length: int
        }

    static member Empty(bg: Color) = { Content = ""; BG = bg; Length = 0 }

    static member (+)(this: CardLine, extra: CardFragment) =
        {
            Content = this.Content + Console.ColorText(extra.Text, extra.FG, extra.BG)
            BG = this.BG
            Length = this.Length + extra.Text.Length
        }

    static member Create(bg: Color) = List.fold (+) (CardLine.Empty(bg))

type CardSide =
    private
        {
            Lines: CardLine list
        }

    static member Empty = { Lines = [] }
    static member (+)(this: CardSide, line: CardLine) = { Lines = this.Lines @ [ line ] }
    static member Create = List.fold (+) CardSide.Empty

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
type StudySession(title: string, cards: Card array) =
    let cards = ResizeArray<Card>(cards |> Seq.randomShuffle)

    [<Literal>]
    let CARD_AREA = 20

    [<Literal>]
    let LOG_SIZE = 16

    static let log = ResizeArray()

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
        for l in log do
            MenuRender.WriteLine(l)

    let draw_title () =
        MenuRender.Write($" Loana: {title} ".PadRight(MenuRender.Width - 16), Color.White, Color.FromArgb(0xFF_303030))
        MenuRender.Write((sprintf " % 2i cards left " (cards.Count + 1)), Color.LightGreen, Color.FromArgb(0xFF_303030))
        MenuRender.WriteLine()

    member this.Start() : StudySessionResult =
        let buttons = [| 0; 0; 0; 0 |]
        let mutable end_early = false

        while cards.Count > 0 do
            let current = cards.[0]
            cards.RemoveAt(0)

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
                    cards.Clear()
                    end_early <- true
                | _ -> ()

            if not end_early then
                draw_title()
                draw_card back
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
                        cards.Clear()
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

    member this.ReplaceNear(card: Card) = cards.Insert(min 4 cards.Count, card)

    member this.ReplaceFar(card: Card) = cards.Add(card)

    member this.Log(message: string) =
        Console.WriteLine(message)
        log.Add(message)

        if log.Count > LOG_SIZE then
            log.RemoveAt(0)

    abstract member Forget: Card -> unit
    abstract member Demote: Card -> unit
    abstract member Keep: Card -> unit
    abstract member Promote: Card -> unit
    abstract member Render: Card -> CardSide * CardSide

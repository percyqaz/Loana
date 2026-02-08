namespace Loana.CLI

open System
open System.Drawing
open Loana.CLI

type CliCard =
    {
        Front: unit -> unit
        Back: unit -> unit
        Answer: string
    }

type CliReviewSession(cards: CliCard array) =

    let cards = ResizeArray<CliCard>(cards |> Seq.randomShuffle)

    member this.Start() =
        while cards.Count > 0 do
            let next = cards.[0]
            cards.RemoveAt(0)

            Console.Clear()
            next.Front()

            if Console.ReadLine() <> next.Answer then
                Console.WriteLine(" Mistake! See below: ", Color.Black, Color.Red)
                next.Back()
                cards.Insert(min 5 cards.Count, next)
                Console.ReadLine() |> ignore

type CardFragment = internal { Text: string; FG: Color; BG: Color }
type CardLine =
    private { Content: string; BG: Color; Length: int }
    static member Empty(bg: Color) = { Content = ""; BG = bg; Length = 0 }
    static member (+) (this: CardLine, extra: CardFragment) =
        { Content = this.Content + Console.ColorText(extra.Text, extra.FG, extra.BG); BG = this.BG; Length = this.Length + extra.Text.Length }
    static member Create(bg: Color) = List.fold (+) (CardLine.Empty(bg))

type CardSide =
    private { Lines: CardLine list; Width: int }
    static member Empty = { Lines = []; Width = 0 }
    static member (+) (this: CardSide, line: CardLine) =
        { Lines = this.Lines @ [line]; Width = max this.Width line.Length }
    static member Create = List.fold (+) CardSide.Empty

type CardMeta = { Key: string; Tier: int }
type Card =
    {
        Meta: CardMeta
        Front: unit -> CardSide
        Back: unit -> CardSide
    }
    member this.Key = this.Meta.Key

[<AbstractClass>]
type StudySession(cards: Card array) =
    let cards = ResizeArray<Card>(cards |> Seq.randomShuffle)

    let CARD_AREA = 20
    let LOG_SIZE = 20

    let log = ResizeArray()
    let render = MenuRender()

    let draw_card(side: CardSide) =
        let edges_width = MenuRender.Width - 12
        let inner_width = edges_width - 4
        let empty() = render.WriteLine(MenuRender.Pad "", Color.White, Color.FromArgb(0x101010))
        let horizontal_edge() =
            render.Write("      ", Color.White, Color.FromArgb(0x101010))
            render.Write("".PadRight(edges_width), Color.White, Color.FromArgb(0x303030))
            render.Write("      ", Color.White, Color.FromArgb(0x101010))
            render.WriteLine()
        let mutable i = 5
        let line(line: CardLine) =
            render.Write("      ", Color.White, Color.FromArgb(0x101010))
            render.Write("  ", Color.White, Color.FromArgb(0x303030))

            render.Write(" ", Color.White, line.BG)
            render.Write(line.Content)
            render.Write("".PadLeft(inner_width - 1 - line.Length |> max 0), Color.White, line.BG)

            render.Write("  ", Color.White, Color.FromArgb(0x303030))
            render.Write("      ", Color.White, Color.FromArgb(0x101010))
            render.WriteLine()
            i <- i + 1
        empty()
        empty()
        horizontal_edge()
        side.Lines |> List.iter line
        horizontal_edge()
        for i = i to CARD_AREA do
            empty()

    let draw_log() =
        render.WriteLine(MenuRender.Pad " - Log - ", Color.LightGray, Color.FromArgb(0x202020))
        for l in log do
            render.WriteLine(l)

    member this.Start() =
        while cards.Count > 0 do
            let current = cards.[0]
            cards.RemoveAt(0)

            let front = current.Front()
            let back = current.Back()

            render.WriteLine(MenuRender.Pad "Review session", Color.White, Color.FromArgb(0x303030))
            draw_card front
            render.WriteLine(MenuRender.Pad "[Space] Reveal", Color.LightGray, Color.FromArgb(0x303030))
            draw_log()
            render.Redraw()

            let mutable loop = true
            let mutable end_early = false
            while loop do
                match Console.ReadKey(true).Key with
                | ConsoleKey.Spacebar -> loop <- false
                | ConsoleKey.Escape -> loop <- false; cards.Clear(); end_early <- true
                | _ -> ()

            if not end_early then
                render.WriteLine(MenuRender.Pad "Review session", Color.White, Color.FromArgb(0x303030))
                draw_card back
                render.Write(" [Z] Forgot ", Color.LightGray, Color.FromArgb(0x303030))
                render.WriteLine(" [,] -1 Level [.] Keep Level [/] +1 Level ".PadLeft(MenuRender.Width - 12), Color.LightGray, Color.FromArgb(0x303030))
                draw_log()
                render.Redraw()

                let mutable loop = true
                while loop do
                    match Console.ReadKey(true).Key with
                    | ConsoleKey.Escape -> cards.Clear(); loop <- false
                    | ConsoleKey.Z -> this.Forget current; loop <- false
                    | ConsoleKey.OemComma -> this.Demote current; loop <- false
                    | ConsoleKey.OemPeriod -> this.Keep current; loop <- false
                    | ConsoleKey.Oem2 -> this.Promote current; loop <- false
                    | _ -> ()

        Console.WriteLine(MenuRender.Pad "Session ended.", Color.LightGreen, Color.FromArgb(0x303030))
        Console.ReadKey(true) |> ignore

    member this.ReplaceNear(card: Card) =
        cards.Insert(min 5 cards.Count, card)

    member this.ReplaceFar(card: Card) =
        cards.Add(card)

    member this.Log(string: string) =
        log.Add(string)
        if log.Count > LOG_SIZE then log.RemoveAt(0)

    abstract member Forget: Card -> unit
    abstract member Demote: Card -> unit
    abstract member Keep: Card -> unit
    abstract member Promote: Card -> unit
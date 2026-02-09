namespace Loana.CLI

open System
open System.Drawing
open Loana.CLI

type QuizCard =
    {
        Front: CardLine list
        Back: CardLine list
        Answer: string
    }

type QuizSession(title: string, cards: QuizCard array) =
    let cards = ResizeArray<QuizCard>(cards |> Seq.randomShuffle)

    let CARD_AREA = 20
    let LOG_SIZE = 20

    let log = ResizeArray()
    let edges_width = MenuRender.Width - 12
    let inner_width = edges_width - 4

    let ANSWER_BG = Color.FromArgb(0x202020)

    let empty() = MenuRender.WriteLine(MenuRender.Pad "", Color.White, Color.FromArgb(0x101010))
    let horizontal_edge() =
        MenuRender.Write("      ", Color.White, Color.FromArgb(0x101010))
        MenuRender.Write("".PadRight(edges_width), Color.White, Color.FromArgb(0x303030))
        MenuRender.Write("      ", Color.White, Color.FromArgb(0x101010))
        MenuRender.WriteLine()
    let line(i: int ref) (line: CardLine) =
        MenuRender.Write("      ", Color.White, Color.FromArgb(0x101010))
        MenuRender.Write("  ", Color.White, Color.FromArgb(0x303030))

        MenuRender.Write(" ", Color.White, line.BG)
        MenuRender.Write(line.Content)
        MenuRender.Write("".PadLeft(inner_width - 1 - line.Length |> max 0), Color.White, line.BG)

        MenuRender.Write("  ", Color.White, Color.FromArgb(0x303030))
        MenuRender.Write("      ", Color.White, Color.FromArgb(0x101010))
        MenuRender.WriteLine()
        i.Value <- i.Value + 1

    let draw_front(front: CardLine list) : int * int =
        empty()
        empty()
        horizontal_edge()
        let i = ref 5
        front |> List.iter (line i)
        line i (CardLine.Create ANSWER_BG [])
        MenuRender.Write("      ", Color.White, Color.FromArgb(0x101010))
        MenuRender.Write("  ", Color.White, Color.FromArgb(0x303030))
        MenuRender.Write(" ", Color.White, ANSWER_BG)
        MenuRender.Write("".PadLeft(inner_width - 2), Color.White, Color.Black)
        MenuRender.Write(" ", Color.White, ANSWER_BG)
        MenuRender.Write("  ", Color.White, Color.FromArgb(0x303030))
        MenuRender.Write("      ", Color.White, Color.FromArgb(0x101010))
        MenuRender.WriteLine()
        line i (CardLine.Create ANSWER_BG [])
        horizontal_edge()
        for _ = i.Value to CARD_AREA do
            empty()
        (9, i.Value - 2)

    let draw_back(front: CardLine list, back: CardLine list, input: string) =
        empty()
        empty()
        horizontal_edge()
        let i = ref 5
        front |> List.iter (line i)
        line i (CardLine.Create ANSWER_BG [])
        MenuRender.Write("      ", Color.White, Color.FromArgb(0x101010))
        MenuRender.Write("  ", Color.White, Color.FromArgb(0x303030))
        MenuRender.Write(" ", Color.White, ANSWER_BG)
        MenuRender.Write(input, Color.White, Color.Black)
        MenuRender.Write("".PadLeft(inner_width - 2 - input.Length), Color.White, Color.Black)
        MenuRender.Write(" ", Color.White, ANSWER_BG)
        MenuRender.Write("  ", Color.White, Color.FromArgb(0x303030))
        MenuRender.Write("      ", Color.White, Color.FromArgb(0x101010))
        MenuRender.WriteLine()
        back |> List.iter (line i)
        horizontal_edge()
        for _ = i.Value to CARD_AREA do
            empty()

    let draw_log() =
        for l in log do
            MenuRender.WriteLine(l)

    let draw_title() =
        MenuRender.Write($" Loana: {title} ".PadRight(MenuRender.Width - 16), Color.White, Color.FromArgb(0x303030))
        MenuRender.Write((sprintf " % 2i cards left " (cards.Count + 1)), Color.LightGreen, Color.FromArgb(0x303030))
        MenuRender.WriteLine()

    member this.Start() =
        while cards.Count > 0 do
            let current = cards.[0]
            cards.RemoveAt(0)

            draw_title()
            let (x, y) = draw_front current.Front
            MenuRender.WriteLine(MenuRender.Pad "[Enter] Submit", Color.LightGray, Color.FromArgb(0x303030))
            draw_log()
            MenuRender.Redraw()
            Console.SetCursorPosition(x, y)

            let input = Console.ReadLine()
            if input <> current.Answer then

                draw_title()
                draw_back(current.Front, current.Back, input)
                MenuRender.WriteLine(MenuRender.Pad " [Enter] Continue ", Color.LightGray, Color.FromArgb(0x303030))
                draw_log()
                MenuRender.Redraw()

                let mutable loop = true
                while loop do
                    match Console.ReadKey(true).Key with
                    | ConsoleKey.Escape -> cards.Clear(); loop <- false
                    | ConsoleKey.Enter -> this.ReplaceNear current; loop <- false
                    | _ -> ()

        Console.WriteLine(MenuRender.Pad " Session ended. ", Color.LightGreen, Color.FromArgb(0x303030))
        Console.ReadKey(true) |> ignore

    member this.ReplaceNear(card: QuizCard) =
        cards.Insert(min 4 cards.Count, card)

    member this.Log(message: string) =
        Console.WriteLine(message)
        log.Add(message)
        if log.Count > LOG_SIZE then log.RemoveAt(0)
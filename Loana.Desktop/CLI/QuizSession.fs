namespace Loana.Desktop.CLI

open System
open System.Drawing
open Loana.Desktop.CLI

type QuestionFragment = internal { Text: string; FG: Color }

type QuestionLine =
    private
        {
            Content: QuestionFragment list
            Length: int
        }

    static member Empty = { Content = []; Length = 0 }

    static member (+)(this: QuestionLine, extra: QuestionFragment) =
        { Content = this.Content @ [ extra ]; Length = this.Length + extra.Text.Length }

    static member Create = List.fold (+) QuestionLine.Empty

type QuestionSide = { BG: Color; Lines: QuestionLine list }

type Question = { Front: QuestionSide; Back: QuestionSide; Answer: string }

type QuizSession(title: string, questions: Question array) =
    let questions = ResizeArray<Question>(questions |> Seq.randomShuffle)

    [<Literal>]
    let QUESTION_AREA = 20

    [<Literal>]
    let LOG_SIZE = 16

    let log = ResizeArray()
    let edges_width = MenuRender.Width - 12
    let inner_width = edges_width - 4
    let mutable mistakes = 0

    let empty () =
        MenuRender.WriteLine(MenuRender.Pad(""), Color.White, Color.FromArgb(0xFF_101010))

    let horizontal_edge () =
        MenuRender.Write("      ", Color.White, Color.FromArgb(0xFF_101010))
        MenuRender.Write("".PadRight(edges_width), Color.White, Color.FromArgb(0xFF_303030))
        MenuRender.Write("      ", Color.White, Color.FromArgb(0xFF_101010))
        MenuRender.WriteLine()

    let line (bg: Color) (line: QuestionLine) =
        MenuRender.Write("      ", Color.White, Color.FromArgb(0xFF_101010))
        MenuRender.Write("  ", Color.White, Color.FromArgb(0xFF_303030))

        MenuRender.Write(" ", Color.White, bg)

        for p in line.Content do
            MenuRender.Write(p.Text, p.FG, bg)

        MenuRender.Write("".PadLeft(inner_width - 1 - line.Length |> max 0), Color.White, bg)

        MenuRender.Write("  ", Color.White, Color.FromArgb(0xFF_303030))
        MenuRender.Write("      ", Color.White, Color.FromArgb(0xFF_101010))
        MenuRender.WriteLine()

    let draw_front (front: QuestionSide, back: QuestionSide) : int * int =
        empty()
        empty()
        horizontal_edge()
        front.Lines |> List.iter(line front.BG)
        line back.BG (QuestionLine.Create([]))
        MenuRender.Write("      ", Color.White, Color.FromArgb(0xFF_101010))
        MenuRender.Write("  ", Color.White, Color.FromArgb(0xFF_303030))
        MenuRender.Write(" ", Color.White, back.BG)
        MenuRender.Write("".PadLeft(inner_width - 2), Color.White, Color.Black)
        MenuRender.Write(" ", Color.White, back.BG)
        MenuRender.Write("  ", Color.White, Color.FromArgb(0xFF_303030))
        MenuRender.Write("      ", Color.White, Color.FromArgb(0xFF_101010))
        MenuRender.WriteLine()
        back.Lines |> List.iter(fun _ -> line back.BG (QuestionLine.Create([])))
        horizontal_edge()
        let i = 5 + front.Lines.Length + 1 + back.Lines.Length

        for _ = i to QUESTION_AREA do
            empty()

        (9, i - 1 - back.Lines.Length)

    let draw_back (front: QuestionSide, back: QuestionSide, input: string) =
        empty()
        empty()
        horizontal_edge()
        front.Lines |> List.iter(line front.BG)
        line back.BG (QuestionLine.Create([]))
        MenuRender.Write("      ", Color.White, Color.FromArgb(0xFF_101010))
        MenuRender.Write("  ", Color.White, Color.FromArgb(0xFF_303030))
        MenuRender.Write(" ", Color.White, back.BG)
        MenuRender.Write(input, Color.White, Color.Black)
        MenuRender.Write("".PadLeft(inner_width - 2 - input.Length), Color.White, Color.Black)
        MenuRender.Write(" ", Color.White, back.BG)
        MenuRender.Write("  ", Color.White, Color.FromArgb(0xFF_303030))
        MenuRender.Write("      ", Color.White, Color.FromArgb(0xFF_101010))
        MenuRender.WriteLine()
        back.Lines |> List.iter(line back.BG)
        horizontal_edge()
        let i = 5 + front.Lines.Length + 1 + back.Lines.Length

        for _ = i to QUESTION_AREA do
            empty()

    let draw_log () =
        for l in log do
            MenuRender.WriteLine(l)

    let draw_title () =
        MenuRender.Write($" Loana: {title} ".PadRight(MenuRender.Width - 16), Color.White, Color.FromArgb(0xFF_303030))

        MenuRender.Write(
            (sprintf " % 2i cards left " (questions.Count + 1)),
            Color.LightGreen,
            Color.FromArgb(0xFF_303030)
        )

        MenuRender.WriteLine()

    member this.Start() : int option =
        let mutable quit_early = false

        while questions.Count > 0 do
            let current = questions.[0]
            questions.RemoveAt(0)

            draw_title()
            let x, y = draw_front(current.Front, current.Back)
            MenuRender.WriteLine(MenuRender.Pad("[Enter] Submit"), Color.LightGray, Color.FromArgb(0xFF_303030))
            draw_log()
            MenuRender.Redraw()
            let struct (x2, y2) = Console.GetCursorPosition()
            Console.SetCursorPosition(x, y)

            let input = Console.ReadLine()
            Console.SetCursorPosition(x2, y2)

            if input <> current.Answer then

                draw_title()
                draw_back(current.Front, current.Back, input)
                MenuRender.WriteLine(MenuRender.Pad(" [Enter] Continue "), Color.LightGray, Color.FromArgb(0xFF_303030))
                draw_log()
                MenuRender.Redraw()

                let mutable loop = true

                while loop do
                    match Console.ReadKey(true).Key with
                    | ConsoleKey.Escape ->
                        questions.Clear()
                        quit_early <- true
                        loop <- false
                    | ConsoleKey.Enter ->
                        this.ReplaceNear(current)
                        loop <- false
                    | _ -> ()

        Console.WriteLine(
            MenuRender.Pad($" Session ended. {mistakes} mistakes! "),
            Color.LightGreen,
            Color.FromArgb(0xFF_202020)
        )

        let mutable result = None

        if quit_early then
            Console.ReadKey(true) |> ignore
        else
            Console.WriteLine(
                " [,] -1 Level [.] Keep Level [/] +1 Level ".PadLeft(MenuRender.Width),
                Color.LightGray,
                Color.FromArgb(0xFF_303030)
            )

            while result.IsNone do
                match Console.ReadKey(true).Key with
                | ConsoleKey.OemComma -> result <- Some -1
                | ConsoleKey.OemPeriod -> result <- Some 0
                | ConsoleKey.Oem2
                | ConsoleKey.Divide -> result <- Some 1
                | _ -> ()

        result

    member this.ReplaceNear(card: Question) =
        mistakes <- mistakes + 1
        questions.Insert(min 4 questions.Count, card)

    member this.Log(message: string) =
        Console.WriteLine(message)
        log.Add(message)

        if log.Count > LOG_SIZE then
            log.RemoveAt(0)

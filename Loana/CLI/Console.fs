namespace Loana.CLI

open System.Drawing

type Console =
    static member ColorText(text: string, fg: Color, bg: Color) = sprintf "\u001b[38;2;%d;%d;%d;48;2;%d;%d;%dm%s\u001b[0m" fg.R fg.G fg.B bg.R bg.G bg.B text

    static member Clear() = System.Console.Clear()

    static member Write(text: string, fg: Color, bg: Color) = System.Console.Write (Console.ColorText(text, fg, bg))
    static member Write(text: string, fg: Color) = Console.Write(text, fg, Color.Black)
    static member Write(text: string) = Console.Write(text, Color.White, Color.Black)

    static member WriteLine(text: string, color: Color, background: Color) = Console.Write(text + "\n", color, background)
    static member WriteLine(text: string, color: Color) = Console.WriteLine(text, color, Color.Black)
    static member WriteLine(text: string) = Console.WriteLine(text, Color.White, Color.Black)
    static member WriteLine() = Console.WriteLine("", Color.White, Color.Black)

type MenuRender() =

    static let mutable width = 103
    let buffer = System.Text.StringBuilder()

    member this.FlushInline() : unit =
        System.Console.Write(buffer.ToString())
        buffer.Clear() |> ignore

    member this.Redraw() : unit =
        System.Console.SetCursorPosition(0, 0)
        this.FlushInline()
        let struct (original_left, original_top) = System.Console.GetCursorPosition()
        let blank_line = String.replicate width " "
        for i = original_top to System.Console.WindowHeight - 1 do
            System.Console.SetCursorPosition(0, i)
            System.Console.Write(blank_line)
        System.Console.SetCursorPosition(original_left, original_top)

    member this.Write(text: string, fg: Color, bg: Color) = buffer.Append(Console.ColorText(text, fg, bg)) |> ignore
    member this.Write(text: string, fg: Color) = this.Write(text, fg, Color.Black)
    member this.Write(text: string) = this.Write(text, Color.White, Color.Black)

    member this.WriteLine(text: string, color: Color, background: Color) = this.Write(text + "\n", color, background)
    member this.WriteLine(text: string, color: Color) = this.WriteLine(text, color, Color.Black)
    member this.WriteLine(text: string) = this.WriteLine(text, Color.White, Color.Black)
    member this.WriteLine() = this.WriteLine("", Color.White, Color.Black)

    static member Pad(text: string) = text.PadLeft(MenuRender.Width / 2 + text.Length / 2).PadRight(MenuRender.Width)

    static member Width = width
    static member UpdateWidth() =
        let old_width = width
        // todo on linux: System.Environment.GetEnvironmentVariable("COLUMNS") |> printfn "%A"
        width <- System.Console.WindowWidth / 2 * 2 - 1
        if old_width <> width then Console.Clear()
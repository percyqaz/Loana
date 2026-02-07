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

type DeferConsole =
    static let buffer = System.Text.StringBuilder()

    static member Flush() : unit =
        System.Console.Write(buffer.ToString())
        buffer.Clear() |> ignore

    static member Redraw() : unit =
        System.Console.SetCursorPosition(0, 0)
        DeferConsole.Flush()
        let struct (left, top) = System.Console.GetCursorPosition()
        let blank_line = String.replicate System.Console.WindowWidth " "
        for i = top to System.Console.WindowHeight - 1 do
            System.Console.SetCursorPosition(left, i)
            System.Console.Write(blank_line)
        System.Console.SetCursorPosition(left, top)

    static member Write(text: string, fg: Color, bg: Color) = buffer.Append(Console.ColorText(text, fg, bg)) |> ignore
    static member Write(text: string, fg: Color) = DeferConsole.Write(text, fg, Color.Black)
    static member Write(text: string) = DeferConsole.Write(text, Color.White, Color.Black)

    static member WriteLine(text: string, color: Color, background: Color) = DeferConsole.Write(text + "\n", color, background)
    static member WriteLine(text: string, color: Color) = DeferConsole.WriteLine(text, color, Color.Black)
    static member WriteLine(text: string) = DeferConsole.WriteLine(text, Color.White, Color.Black)
    static member WriteLine() = DeferConsole.WriteLine("", Color.White, Color.Black)
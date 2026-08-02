namespace Loana.Desktop.CLI

open System.Drawing

type Console =
    static member ColorText(text: string, fg: Color, bg: Color) : string =
        if bg.A > 0uy then
            sprintf "\u001b[38;2;%d;%d;%d;48;2;%d;%d;%dm%s\u001b[0m" fg.R fg.G fg.B bg.R bg.G bg.B text
        else
            sprintf "\u001b[38;2;%d;%d;%dm%s\u001b[39m" fg.R fg.G fg.B text

    static member Clear() : unit = System.Console.Clear()

    static member Write(text: string, fg: Color, bg: Color) : unit =
        System.Console.Write(Console.ColorText(text, fg, bg))

    static member Write(text: string, fg: Color) : unit =
        Console.Write(text, fg, Color.Transparent)

    static member Write(text: string) : unit =
        Console.Write(text, Color.White, Color.Transparent)

    static member WriteLine(text: string, color: Color, background: Color) : unit =
        Console.Write(text + "\n", color, background)

    static member WriteLine(text: string, color: Color) : unit =
        Console.WriteLine(text, color, Color.Transparent)

    static member WriteLine(text: string) : unit =
        Console.WriteLine(text, Color.White, Color.Transparent)

    static member WriteLine() : unit =
        Console.WriteLine("", Color.White, Color.Transparent)

/// Deferred block rendering for UIs
type MenuRender =

    static let mutable width = 103
    static let buffer = System.Text.StringBuilder()

    static member FlushInline() : unit =
        System.Console.Write(buffer.ToString())
        buffer.Clear() |> ignore

    static member Redraw() : unit =
        let draw_buffer_top () =
            System.Console.SetCursorPosition(0, 0)
            MenuRender.FlushInline()

        let fill_screen_blank_lines () =
            let struct (original_left, original_top) = System.Console.GetCursorPosition()

            let blank_line = String.replicate width " "

            for i = original_top to System.Console.WindowHeight - 1 do
                System.Console.SetCursorPosition(0, i)
                System.Console.Write(blank_line)

            System.Console.SetCursorPosition(original_left, original_top)

        draw_buffer_top()
        fill_screen_blank_lines()

    static member Write(text: string, fg: Color, bg: Color) : unit =
        buffer.Append(Console.ColorText(text, fg, bg)) |> ignore

    static member Write(text: string, fg: Color) : unit =
        MenuRender.Write(text, fg, Color.Transparent)

    static member Write(text: string) : unit =
        MenuRender.Write(text, Color.White, Color.Transparent)

    static member WriteLine(text: string, color: Color, background: Color) : unit =
        MenuRender.Write(text + "\n", color, background)

    static member WriteLine(text: string, color: Color) : unit =
        MenuRender.WriteLine(text, color, Color.Transparent)

    static member WriteLine(text: string) : unit =
        MenuRender.WriteLine(text, Color.White, Color.Transparent)

    static member WriteLine() : unit =
        MenuRender.WriteLine("", Color.White, Color.Transparent)

    static member Pad(text: string) : string =
        text.PadLeft(MenuRender.Width / 2 + text.Length / 2).PadRight(MenuRender.Width)

    static member FormatInterval(seconds: int64) : string =
        let total_minutes = seconds / System.TimeSpan.SecondsPerMinute
        let minutes = total_minutes % 60L
        let hours = (total_minutes / 60L) % 24L
        let days = (total_minutes / System.TimeSpan.MinutesPerDay)
        if days > 0 then sprintf "%02id%02ih%02im" days hours minutes else sprintf "%02ih%02im" hours minutes

    static member Width = width

    static member UpdateWidth() : unit =
        let old_width = width
        width <- System.Console.WindowWidth / 2 * 2 - 1

        if old_width <> width then
            Console.Clear()

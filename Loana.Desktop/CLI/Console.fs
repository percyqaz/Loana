namespace Loana.Desktop.CLI

open System.Drawing

/// Deferred block rendering for UIs
type MenuRender =

    static let mutable width = 103
    static let buffer = System.Text.StringBuilder()

    static member FlushInline() : unit =
        System.Console.Write(buffer.Append(AnsiCodes.ClearRestOfScreen).ToString())
        buffer.Clear() |> ignore

    static member Redraw() : unit =
        System.Console.SetCursorPosition(0, 0)
        MenuRender.FlushInline()

    static member Write(text: string, fg: Color, bg: Color) : unit =
        if bg.A <> 0uy then
            buffer.Append(text.ForeColor(fg).BackColor(bg)) |> ignore
        else
            buffer.Append(text.ForeColor(fg)) |> ignore

    static member Write(text: string, fg: Color) : unit =
        MenuRender.Write(text, fg, Color.Transparent)

    static member Write(text: string) : unit =
        MenuRender.Write(text, Color.White, Color.Transparent)

    static member WriteLine(text: string, color: Color, background: Color) : unit =
        MenuRender.Write(text.ClearRestOfLine() + "\n", color, background)

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
        width <- System.Console.WindowWidth / 2 * 2 - 1

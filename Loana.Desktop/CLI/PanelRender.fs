namespace Loana.Desktop.CLI

open System.Text

type PanelRender(initial: string, line_prefix: string, line_suffix: string) =

    let sb = StringBuilder().Append(initial)
    let mutable is_empty_line = true

    member this.Write(text: string) : unit =
        if is_empty_line then
            is_empty_line <- false
            sb.Append(line_prefix) |> ignore

        sb.Append(text.Replace("\n", line_suffix + "\n")) |> ignore

        if text.Contains('\n') then
            is_empty_line <- true

    override this.ToString() : string = sb.ToString()

    static member Left() : PanelRender =
        PanelRender(AnsiCodes.CursorToOrigin, "", "")

    static member Right() : PanelRender =
        PanelRender(AnsiCodes.CursorToOrigin, AnsiCodes.CursorRight((MenuRender.Width + 1) / 2), "")

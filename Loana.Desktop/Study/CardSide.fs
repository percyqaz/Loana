namespace Loana.Desktop.Study

open System.Drawing
open Loana.Desktop.CLI

type CardLine =
    private
        {
            Content: string
            Length: int
        }

    static member internal Empty: CardLine = { Content = ""; Length = 0 }

    member this.Append(text: string, format: string -> string) : CardLine =
        { Content = this.Content + format(text); Length = this.Length + text.Length }

    member this.Prepend(text: string, format: string -> string) : CardLine =
        { Content = format(text) + this.Content; Length = this.Length + text.Length }

    static member Append(text: string, format: string -> string) : CardLine = CardLine.Empty.Append(text, format)

    member this.Render(width: int) : string =
        (" " + this.Content) + String.replicate (width - this.Length - 1 |> max 0) " "

type CardSection =
    private
        {
            Lines: CardLine list
            Background: Color
        }

    static member Create(bg: Color, lines: CardLine list) : CardSection =
        { Lines = [ CardLine.Empty ] @ lines @ [ CardLine.Empty ]; Background = bg }

    static member Create(bg: Color, line: CardLine) : CardSection =
        { Lines = [ CardLine.Empty; line; CardLine.Empty ]; Background = bg }

    member this.Render(width: int) : string array =
        this.Lines |> Seq.map _.Render(width).BackColor(this.Background) |> Array.ofSeq

type CardSide =
    private
        {
            Sections: CardSection list
        }

    static let FRAME_COLOR = 0xFF_202020
    static member Create(sections: CardSection list) : CardSide = { Sections = sections }

    member this.Render(width_with_frame: int) : string array =
        let top_bottom = "".PadRight(width_with_frame).BackColor(FRAME_COLOR)

        seq {
            yield top_bottom

            yield!
                this.Sections
                |> Seq.collect _.Render(width_with_frame - 4)
                |> Seq.map(fun s -> ("  ".BackColor(FRAME_COLOR) + s + "  ".BackColor(FRAME_COLOR)))

            yield top_bottom
        }
        |> Array.ofSeq

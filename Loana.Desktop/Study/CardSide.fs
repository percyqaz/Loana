namespace Loana.Desktop.Study

open System.Drawing
open Loana.Desktop.CLI

type CardFragment =
    internal
        {
            Text: string
            FG: Color
            BG: Color
        }

    override this.ToString() : string =
        this.Text.ForeColor(this.FG).BackColor(this.BG)

type CardLine =
    private
        {
            Content: string
            BG: Color
            Length: int
        }

    static member Empty(bg: Color) : CardLine = { Content = ""; BG = bg; Length = 0 }

    static member (+)(this: CardLine, extra: CardFragment) : CardLine =
        { Content = this.Content + extra.ToString(); BG = this.BG; Length = this.Length + extra.Text.Length }

    static member Create(bg: Color) : _ = List.fold (+) (CardLine.Empty(bg))

type CardSide =
    private
        {
            Lines: CardLine list
        }

    static member Empty = { Lines = [] }
    static member (+)(this: CardSide, line: CardLine) : CardSide = { Lines = this.Lines @ [ line ] }
    static member Create = List.fold (+) CardSide.Empty

namespace Loana.Language

open System.Drawing
open System.Runtime.CompilerServices

[<RequireQualifiedAccess>]
type Case =
    | Nominative
    | Accusative
    | Dative
    | Genitive

    override this.ToString() : string =
        match this with
        | Nominative -> "nom"
        | Accusative -> "acc"
        | Dative -> "dat"
        | Genitive -> "gen"

    member this.Color: Color =
        match this with
        | Nominative -> Color.Green
        | Accusative -> Color.Cyan
        | Dative -> Color.DarkMagenta
        | Genitive -> Color.Gold

    static member LIST: Case list = [ Nominative; Accusative; Dative; Genitive ]

    [<Extension>]
    static member RemoveCaseAnnotations(text: string) : string =
        text.Replace("+D", "").Replace("+A", "").Replace("+N", "").Replace("+G", "")

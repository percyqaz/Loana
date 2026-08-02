namespace Loana.Language

open System.Drawing

[<RequireQualifiedAccess>]
type Gender =
    | Masculine
    | Feminine
    | Neuter
    | Plural

    override this.ToString() : string =
        match this with
        | Masculine -> "m"
        | Feminine -> "f"
        | Neuter -> "n"
        | Plural -> "p"

    static member FromString(value: string) : Gender =
        match value with
        | "m" -> Masculine
        | "f" -> Feminine
        | "n" -> Neuter
        | "p" -> Plural
        | _ -> failwithf "could not parse gender from '%s'" value

    member this.Color: Color =
        match this with
        | Masculine -> Color.FromArgb(0xFF_90A0E0)
        | Neuter -> Color.LightGreen
        | Feminine -> Color.FromArgb(0xFF_E090C0)
        | Plural -> Color.FromArgb(0xFF_E0E090)

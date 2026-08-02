namespace Loana.Language

open System.Text.RegularExpressions

type Annotation =
    {
        Text: string
        Note: string option
    }

    override this.ToString() : string =
        match this.Note with
        | Some note -> sprintf "%s [%s]" this.Text note
        | None -> this.Text

    static member FromString(value: string) : Annotation =
        let regex_match = Regex.Match(value, "([^\[]+?)(\s*\[(.*?)\]\s*)?$")
        let note = regex_match.Groups.[3].Value
        let text = regex_match.Groups.[1].Value

        let optional_note = if note = "" then None else Some note

        if text = "" then
            failwithf "Parsing '%s' as an annotation failed" value

        { Text = text; Note = optional_note }

namespace Loana.Desktop.CLI

open System.Drawing
open System.Runtime.CompilerServices
open Loana.Desktop.CLI
open Loana.Language
open Loana.Data

type VocabExtensions =

    [<Extension>]
    static member HighlightString(this: Annotation) : string =
        match this.Note with
        | Some note -> this.Text + " " + ("[" + note + "]").ForeColor(Color.LightGray)
        | None -> this.Text

    [<Extension>]
    static member HighlightString(this: Vocab) : string =
        let color =
            if this.LooksLikeAVerb then Color.FromArgb(0xFF_ffddff)
            elif this.LooksLikeANoun then Color.FromArgb(0xFF_ddffdd)
            else Color.White

        this.Deutsch.ForeColor(color).Bold()
        + " = ".ForeColor(Color.LightGray)
        + String.concat ", " (this.English :: this.EnglishAlternatives |> Seq.map _.HighlightString())

    [<Extension>]
    static member HighlightString(this: Noun) : string =
        match this.Guts with
        | Plural -> this.Translation.HighlightString() + " :p".ForeColor(Gender.Plural.Color)
        | Masculine p
        | Feminine p
        | Neuter p ->
            let gender_highlight_string =
                (" :" + this.Guts.Gender.ToString()).ForeColor(this.Guts.Gender.Color)

            match p with
            | KnownValue plural ->
                this.Translation.HighlightString()
                + gender_highlight_string
                + " plural ".ForeColor(Gender.Plural.Color)
                + plural.HighlightString()
            | KnownNothing ->
                this.Translation.HighlightString()
                + gender_highlight_string
                + " no_plural".ForeColor(Gender.Plural.Color)
            | Unknown -> this.Translation.HighlightString() + gender_highlight_string

    [<Extension>]
    static member HighlightString(this: Verb) : string =
        match this.PastParticiple with
        | Unknown -> this.Infinitive.HighlightString()
        | KnownNothing ->
            this.Infinitive.HighlightString()
            + (" :" + String.concat " " (this.Tenses |> List.map(_.ToString())))
                .ForeColor(0xFF_ffddff)
        | KnownValue pp ->
            this.Infinitive.HighlightString()
            + (" :" + String.concat "" (this.Tenses |> List.map(fun x -> x.ToString() + " ")))
                .ForeColor(0xFF_ffddff)
            + "pp ".ForeColor(0xFF_ffdddd)
            + pp.HighlightString()

    [<Extension>]
    static member HighlightString(this: WordlistItem) : string =
        match this with
        | Noun n -> n.HighlightString()
        | Verb v -> v.HighlightString()
        | Vocab v -> v.HighlightString()

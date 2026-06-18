namespace Loana.Desktop.CLI

open System.Drawing
open System.Runtime.CompilerServices
open Loana.Desktop.CLI
open Loana.Language
open Loana.Data

type VocabExtensions =

    [<Extension>]
    static member HighlightString(this: Annotation) =
        match this.Note with
        | Some note ->
            Console.ColorText(this.Text, Color.White, Color.Transparent) +
            " " +
            Console.ColorText("[" + note + "]", Color.LightGray, Color.Transparent)
        | None -> Console.ColorText(this.Text, Color.White, Color.Transparent)

    [<Extension>]
    static member HighlightString(this: Vocab) =
        let color =
            if this.LooksLikeAVerb then Color.FromArgb(0xFF_ffddff)
            elif this.LooksLikeANoun then Color.FromArgb(0xFF_ddffdd)
            else Color.White

        Console.ColorText(this.Deutsch, color, Color.Transparent) +
        Console.ColorText(" = ", Color.LightGray, Color.Transparent) +
        String.concat ", " (this.English :: this.EnglishAlternatives |> Seq.map _.HighlightString())

    [<Extension>]
    static member HighlightString(this: Noun) =
        match this.Guts with
        | Plural ->
            this.Translation.HighlightString() +
            Console.ColorText(" :p", Gender.Plural.Color, Color.Transparent)
        | Masculine p
        | Feminine p
        | Neuter p ->
            let gender_highlight_string = Console.ColorText(" :" + this.Guts.Gender.ToString(), this.Guts.Gender.Color, Color.Transparent)
            match p with
            | KnownValue plural ->
                this.Translation.HighlightString() +
                gender_highlight_string +
                Console.ColorText(" plural ", Gender.Plural.Color, Color.Transparent) +
                plural.HighlightString()
            | KnownNothing ->
                this.Translation.HighlightString() +
                gender_highlight_string +
                Console.ColorText(" no_plural", Gender.Plural.Color, Color.Transparent)
            | Unknown ->
                this.Translation.HighlightString() +
                gender_highlight_string

    [<Extension>]
    static member HighlightString(this: Verb) =
        match this.PastParticiple with
        | Unknown -> this.Infinitive.HighlightString()
        | KnownNothing ->
            this.Infinitive.HighlightString() +
            Console.ColorText(" :" + (String.concat " " (this.Tenses |> List.map (fun x -> x.ToString()))), Color.FromArgb(0xFF_ffddff), Color.Transparent)
        | KnownValue pp ->
            this.Infinitive.HighlightString() +
            Console.ColorText(" :" + (String.concat "" (this.Tenses |> List.map (fun x -> x.ToString() + " "))), Color.FromArgb(0xFF_ffddff), Color.Transparent) +
            Console.ColorText("pp ", Color.FromArgb(0xFF_ffdddd), Color.Transparent) +
            pp.HighlightString()

    [<Extension>]
    static member HighlightString(this: WordlistItem) =
        match this with
        | Noun n -> n.HighlightString()
        | Verb v -> v.HighlightString()
        | Vocab v -> v.HighlightString()
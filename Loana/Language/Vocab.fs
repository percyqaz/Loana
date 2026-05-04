namespace Loana.Language

open System
open System.Drawing
open System.Text.RegularExpressions
open Loana.CLI

module Key =

    let of_german (text: string) =
        text
            .Replace("ö", "oe")
            .Replace("ä", "ae")
            .Replace("ü", "ue")
            .Replace("ß", "ss")
            .Replace("-", "_")
            .Replace(" ", "_")

type Knowledge<'T> =
    | ToBeDetermined
    | Nothing
    | Something of 'T

[<RequireQualifiedAccess>]
type Gender =
    | Masculine
    | Feminine
    | Neuter
    | Plural

    override this.ToString() =
        match this with
        | Masculine -> "m"
        | Feminine -> "f"
        | Neuter -> "n"
        | Plural -> "p"

    member this.Color : Color =
        match this with
        | Masculine -> Color.FromArgb(0x90A0E0)
        | Neuter -> Color.LightGreen
        | Feminine -> Color.FromArgb(0xE090C0)
        | Plural -> Color.FromArgb(0xE0E090)

    static member Parse(string: string) : Gender =
        match string with
        | "m" -> Masculine
        | "f" -> Feminine
        | "n" -> Neuter
        | "p" -> Plural
        | _ -> failwithf "could not parse gender from '%s'" string

[<RequireQualifiedAccess>]
type Case =
    | Nominative
    | Accusative
    | Dative
    | Genitive

    override this.ToString() =
        match this with
        | Nominative -> "nom"
        | Accusative -> "acc"
        | Dative -> "dat"
        | Genitive -> "gen"

    member this.Color : Color =
        match this with
        | Nominative -> Color.Green
        | Accusative -> Color.Cyan
        | Dative -> Color.DarkMagenta
        | Genitive -> Color.Gold

    static member LIST =
        [
            Nominative
            Accusative
            Dative
            Genitive
        ]

[<RequireQualifiedAccess>]
type Person =
    | First of plural: bool
    | Second of plural: bool
    | Third of Gender
    | Formal

    override this.ToString() =
        match this with
        | First false -> "1"
        | First true -> "1p"
        | Second false -> "2"
        | Second true -> "2p"
        | Third g -> "3" + g.ToString()
        | Formal -> "F"

    static member LIST =
        [
            First false
            First true
            Second false
            Second true
            Third Gender.Masculine
            Third Gender.Feminine
            Third Gender.Neuter
            Third Gender.Plural
            Formal
        ]

type Annotation =
    {
        Text: string
        Note: string option
    }
    override this.ToString() =
        match this.Note with
        | Some note -> sprintf "%s [%s]" this.Text note
        | None -> this.Text
        
    member this.HighlightString =
        match this.Note with
        | Some note ->
            Console.ColorText(this.Text, Color.White, Color.Black) +
            " " +
            Console.ColorText("[" + note + "]", Color.LightGray, Color.Black)
        | None -> Console.ColorText(this.Text, Color.White, Color.Black)

    static member Parse(s: string) =
        let m = Regex.Match(s, "([^\[]+?)(\s*\[(.*?)\]\s*)?$")
        let note = match m.Groups.[3].Value with "" -> None | s -> Some s
        match m.Groups.[1].Value with
        | "" -> failwithf "Parsing '%s' as an annotation failed" s
        | text -> { Text = text; Note = note }

type Vocab =
    {
        Deutsch: string
        English: Annotation
        EnglishAlternatives: Annotation list
    }
    override this.ToString() =
        sprintf "%s = %s" this.Deutsch this.EnglishKey
        
    member this.HighlightString =
        let color =
            if this.DetectVerb then Color.FromArgb(0xffddff)
            elif this.DetectNoun then Color.FromArgb(0xddffdd)
            else Color.White
        
        Console.ColorText(this.Deutsch, color, Color.Black) +
        Console.ColorText(" = ", Color.LightGray, Color.Black) +
        String.concat ", " (this.English :: this.EnglishAlternatives |> Seq.map _.HighlightString)

    member this.Key = Key.of_german this.Deutsch
    member this.EnglishKey = (this.English :: this.EnglishAlternatives) |> Seq.map _.ToString() |> String.concat ", "

    member this.DetectVerb = this.English.Text.StartsWith("to ") && this.EnglishAlternatives |> List.forall _.Text.StartsWith("to ")
    member this.DetectNoun = this.Deutsch.Length > 0 && Char.IsUpper(this.Deutsch.[0])

    static member Parse(s: string) =
        let split = s.Split("=", 2, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        if split.Length < 2 then failwithf "Parsing '%s' as vocab failed" s
        let alts = split.[1].Split(",", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        if alts.Length < 1 then failwithf "should be unreachable"
        { Deutsch = split.[0]; English = Annotation.Parse alts.[0]; EnglishAlternatives = Seq.skip 1 alts |> Seq.map Annotation.Parse |> List.ofSeq }

type NounGuts =
    | Masculine of plural: Knowledge<Vocab>
    | Feminine of plural: Knowledge<Vocab>
    | Neuter of plural: Knowledge<Vocab>
    | Plural

    member this.Gender =
        match this with
        | Masculine _ -> Gender.Masculine
        | Feminine _ -> Gender.Feminine
        | Neuter _ -> Gender.Neuter
        | Plural -> Gender.Plural

type Noun =
    {
        Translation: Vocab
        Guts: NounGuts
    }

    member this.Deutsch = this.Translation.Deutsch
    member this.English = this.Translation.English
    member this.EnglishAlternatives = this.Translation.EnglishAlternatives

    member this.Plural : Knowledge<Vocab> =
        match this.Guts with
        | Plural -> Nothing
        | Masculine plural
        | Feminine plural
        | Neuter plural -> plural

    member this.PluralForm : Noun option =
        match this.Plural with
        | Something plural -> Some { Translation = plural; Guts = Plural }
        | _ -> None

    member this.KeyWithGender =
        this.Guts.Gender.ToString() + "_" + Key.of_german this.Deutsch

    override this.ToString() =
        match this.Guts with
        | Plural -> sprintf "%O :p" this.Translation
        | Masculine p
        | Feminine p
        | Neuter p ->
            match p with
            | Something plural -> sprintf "%O :%O plural %O" this.Translation this.Guts.Gender plural
            | Nothing -> sprintf "%O :%O no_plural" this.Translation this.Guts.Gender
            | ToBeDetermined -> sprintf "%O :%O" this.Translation this.Guts.Gender
            
    member this.HighlightString : string =
        match this.Guts with
        | Plural ->
            this.Translation.HighlightString +
            Console.ColorText(" :p", Gender.Plural.Color, Color.Black)
        | Masculine p
        | Feminine p
        | Neuter p ->
            let gender_highlight_string = Console.ColorText(" :" + this.Guts.Gender.ToString(), this.Guts.Gender.Color, Color.Black)
            match p with
            | Something plural ->
                this.Translation.HighlightString +
                gender_highlight_string +
                Console.ColorText(" plural ", Gender.Plural.Color, Color.Black) +
                plural.HighlightString
            | Nothing -> 
                this.Translation.HighlightString +
                gender_highlight_string +
                Console.ColorText(" no_plural", Gender.Plural.Color, Color.Black)
            | ToBeDetermined -> 
                this.Translation.HighlightString +
                gender_highlight_string
        

type Adjective =
    {
        Translation: Vocab
    }

    member this.Deutsch = this.Translation.Deutsch
    member this.English = this.Translation.English
    member this.EnglishAlternatives = this.Translation.EnglishAlternatives

    member this.Key = Key.of_german this.Deutsch
        
[<RequireQualifiedAccess>]
type VerbQuiz =
    | Present
    | SimplePast
    | Imperative
    override this.ToString() =
        match this with
        | Present -> "pr"
        | SimplePast -> "pa"
        | Imperative -> "im"
    static member Parse(value: string) =
        match value with
        | "pr" -> Present
        | "pa" -> SimplePast
        | "im" -> Imperative
        | _ -> failwithf "Unrecognised verb quiz '%s'" value

type Verb =
    {
        Infinitive: Vocab
        PastParticiple: Knowledge<Vocab>
        Dative: bool
        Quizzes: VerbQuiz list
    }
    override this.ToString() =
        match this.PastParticiple with
        | ToBeDetermined -> this.Infinitive.ToString()
        | Nothing -> sprintf "%O :%s" this.Infinitive (String.concat " " (this.Quizzes |> List.map _.ToString()))
        | Something pp -> sprintf "%O :%spp %O" this.Infinitive (String.concat "" (this.Quizzes |> List.map (fun x -> x.ToString() + " "))) pp
        
    member this.HighlightString =
        match this.PastParticiple with
        | ToBeDetermined -> this.Infinitive.HighlightString
        | Nothing ->
            this.Infinitive.HighlightString +
            Console.ColorText(" :" + (String.concat "" (this.Quizzes |> List.map (fun x -> x.ToString() + " "))), Color.FromArgb(0xffddff), Color.Black)
        | Something pp ->
            this.Infinitive.HighlightString +
            Console.ColorText(" :" + (String.concat "" (this.Quizzes |> List.map (fun x -> x.ToString() + " "))), Color.FromArgb(0xffddff), Color.Black) +
            Console.ColorText("pp ", Color.FromArgb(0xffdddd), Color.Black) +
            pp.HighlightString
   
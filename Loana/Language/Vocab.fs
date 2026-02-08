namespace Loana.Language

open System
open System.Drawing
open System.Text.RegularExpressions

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

    member this.Key = Key.of_german this.Deutsch
    member this.EnglishKey = (this.English :: this.EnglishAlternatives) |> Seq.map _.ToString() |> String.concat ", "

    member this.DetectVerb = this.English.Text.StartsWith("to ") && this.EnglishAlternatives |> List.forall (fun x -> x.Text.StartsWith("to "))
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

type Adjective =
    {
        Translation: Vocab
    }

    member this.Deutsch = this.Translation.Deutsch
    member this.English = this.Translation.English
    member this.EnglishAlternatives = this.Translation.EnglishAlternatives

    member this.Key = Key.of_german this.Deutsch

type VerbPerson =
    | FirstSingular
    | FirstThirdPluralFormal
    | SecondSingular
    | SecondPlural
    | ThirdSingular

    static member OfPerson(person: Person) =
        match person with
        | Person.First false -> FirstSingular
        | Person.First true
        | Person.Third Gender.Plural
        | Person.Formal -> FirstThirdPluralFormal
        | Person.Second false -> SecondSingular
        | Person.Second true -> SecondPlural
        | Person.Third _ -> ThirdSingular

type Verb =
    {
        Infinitive: Vocab
        PastParticiple: Knowledge<Vocab>
        Inflections: Vocab list
    }
    member this.WithInflection(vocab: Vocab) = { this with Inflections = this.Inflections @ [vocab] }
    override this.ToString() = this.Infinitive.ToString()
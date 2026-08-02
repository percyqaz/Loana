namespace Loana.Language

open System
open System.Drawing
open System.Text.RegularExpressions

module AsciiIdentifier =

    let from_deutsch (deutsch: string) : string =
        deutsch
            .Replace("ö", "oe")
            .Replace("ä", "ae")
            .Replace("ü", "ue")
            .Replace("ß", "ss")
            .Replace("-", "_")
            .Replace(" ", "_")

type Knowledge<'T> =
    | Unknown
    | KnownNothing
    | KnownValue of 'T

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

[<RequireQualifiedAccess>]
type Person =
    | First of plural: bool
    | Second of plural: bool
    | Third of Gender
    | Formal

    override this.ToString() : string =
        match this with
        | First false -> "1"
        | First true -> "1p"
        | Second false -> "2"
        | Second true -> "2p"
        | Third g -> "3" + g.ToString()
        | Formal -> "F"

    static member LIST: Person list =
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

type Vocab =
    {
        Deutsch: string
        English: Annotation
        EnglishAlternatives: Annotation list
    }

    override this.ToString() : string =
        sprintf "%s = %s" this.Deutsch this.EnglishAsciiIdentifier

    static member FromString(value: string) : Vocab =
        let TRIM_AND_REMOVE_ENTRIES =
            StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries

        let split_by_equals = value.Split('=', 2, TRIM_AND_REMOVE_ENTRIES)

        if split_by_equals.Length < 2 then
            failwithf "Parsing '%s' as vocab failed: no '=' in provided value" value

        let deutsch = split_by_equals.[0]
        let english_alternatives = split_by_equals.[1].Split(',', TRIM_AND_REMOVE_ENTRIES)
        assert (english_alternatives.Length >= 1)

        {
            Deutsch = deutsch
            English = english_alternatives |> Seq.head |> Annotation.FromString
            EnglishAlternatives = english_alternatives |> Seq.tail |> Seq.map Annotation.FromString |> List.ofSeq
        }

    member this.DeutschAsciiIdentifier: string = AsciiIdentifier.from_deutsch this.Deutsch

    member this.EnglishAsciiIdentifier: string =
        (this.English :: this.EnglishAlternatives) |> Seq.map _.ToString() |> String.concat ", "

    member this.LooksLikeAVerb: bool =
        this.English.Text.StartsWith("to ") && this.EnglishAlternatives |> List.forall _.Text.StartsWith("to ")

    member this.LooksLikeANoun: bool =
        this.Deutsch.Length > 0 && Char.IsUpper(this.Deutsch.[0])

[<Struct>]
type internal TaggedVocab =
    {
        Vocab: Vocab
        Tags: string list
    }

    static member FromString(line: string) : TaggedVocab =
        let TRIM_AND_REMOVE_ENTRIES =
            StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries

        if line = "" then
            failwith "Cannot parse empty line as a noun"

        let split_by_colon = line.Split(':', 2, TRIM_AND_REMOVE_ENTRIES)
        let vocab_definition = split_by_colon.[0]

        let tags =
            if split_by_colon.Length = 2 then split_by_colon.[1].Split(' ', TRIM_AND_REMOVE_ENTRIES) else [||]

        { Vocab = Vocab.FromString(vocab_definition); Tags = List.ofArray(tags) }

type NounGuts =
    | Masculine of plural: Knowledge<Vocab>
    | Feminine of plural: Knowledge<Vocab>
    | Neuter of plural: Knowledge<Vocab>
    | Plural

    member this.Gender: Gender =
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

    member this.Deutsch: string = this.Translation.Deutsch
    member this.English: Annotation = this.Translation.English
    member this.EnglishAlternatives: Annotation list = this.Translation.EnglishAlternatives

    member this.Plural: Knowledge<Vocab> =
        match this.Guts with
        | Plural -> KnownNothing
        | Masculine plural
        | Feminine plural
        | Neuter plural -> plural

    member this.PluralForm: Noun option =
        match this.Plural with
        | KnownValue plural -> Some { Translation = plural; Guts = Plural }
        | _ -> None

    member this.AsciiIdentifierWithGender: string =
        this.Guts.Gender.ToString() + "_" + AsciiIdentifier.from_deutsch this.Deutsch

    override this.ToString() : string =
        match this.Guts with
        | Plural -> sprintf "%O :p" this.Translation
        | Masculine p
        | Feminine p
        | Neuter p ->
            match p with
            | KnownValue plural -> sprintf "%O :%O plural %O" this.Translation this.Guts.Gender plural
            | KnownNothing -> sprintf "%O :%O no_plural" this.Translation this.Guts.Gender
            | Unknown -> sprintf "%O :%O" this.Translation this.Guts.Gender

    static member internal FromTaggedVocab(t: TaggedVocab) : Noun =
        let mutable remaining_tags = t.Tags
        let mutable gender: Gender option = None
        let mutable plural: Vocab option = None
        let mutable no_plural: bool = false

        let inline read_tag (next: string) : unit =
            match next with
            | "p"
            | "m"
            | "f"
            | "n" ->
                if gender.IsSome then
                    failwithf "Gender was set twice for noun: %O" t.Vocab

                gender <- Some(Gender.FromString(next))
            | "no_plural" ->
                if gender.IsNone then
                    failwithf "'no_plural' must be set after gender for noun: %O" t.Vocab

                no_plural <- true
            | "plural" ->
                if gender.IsNone then
                    failwithf "plural must be set after gender for noun: %O" t.Vocab

                if no_plural then
                    failwithf "plural conflicts with 'no_plural' for noun: %O" t.Vocab

                plural <- Some(Vocab.FromString(String.concat " " remaining_tags))
                remaining_tags <- []
            | _ -> failwithf "Unrecognised tag '%s' for noun: %O" next t.Vocab

        while remaining_tags <> [] do
            let next = remaining_tags.Head
            remaining_tags <- remaining_tags.Tail
            read_tag(next)

        let inline guts_plural () =
            if no_plural then
                KnownNothing
            else
                match plural with
                | Some p -> KnownValue p
                | None -> Unknown

        {
            Translation = t.Vocab
            Guts =
                match gender with
                | None ->
                    failwithf "No gender was specified for this noun! Got: %O :%s" t.Vocab (String.concat " " t.Tags)
                | Some Gender.Masculine -> Masculine(guts_plural())
                | Some Gender.Feminine -> Feminine(guts_plural())
                | Some Gender.Neuter -> Neuter(guts_plural())
                | Some Gender.Plural -> Plural
        }

    static member FromString(line: string) : Noun =
        Noun.FromTaggedVocab(TaggedVocab.FromString(line))

type Adjective =
    {
        Translation: Vocab
    }

    member this.Deutsch: string = this.Translation.Deutsch
    member this.English: Annotation = this.Translation.English
    member this.EnglishAlternatives: Annotation list = this.Translation.EnglishAlternatives

    member this.AsciiIdentifier: string = AsciiIdentifier.from_deutsch this.Deutsch

[<RequireQualifiedAccess>]
type VerbTense =
    | Present
    | SimplePast
    | Imperative

    override this.ToString() : string =
        match this with
        | Present -> "pr"
        | SimplePast -> "pa"
        | Imperative -> "im"

    static member FromString(value: string) : VerbTense =
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
        Tenses: VerbTense list
    }

    override this.ToString() : string =
        match this.PastParticiple with
        | Unknown -> this.Infinitive.ToString()
        | KnownNothing -> sprintf "%O :%s" this.Infinitive (String.concat " " (this.Tenses |> List.map _.ToString()))
        | KnownValue pp ->
            sprintf
                "%O :%spp %O"
                this.Infinitive
                (String.concat "" (this.Tenses |> List.map(fun x -> x.ToString() + " ")))
                pp

    static member internal FromTaggedVocab(t: TaggedVocab) : Verb =
        let mutable remaining_tags = t.Tags
        let mutable verb_tenses: VerbTense list = []
        let mutable is_dative = false

        let mutable past_participle: Knowledge<Vocab> =
            if t.Tags <> [] then KnownNothing else Unknown

        let inline read_tag (next: string) : unit =
            match next with
            | "pa"
            | "pr"
            | "im" -> verb_tenses <- verb_tenses @ [ VerbTense.FromString(next) ]
            | "dat" ->
                if is_dative then
                    failwith "Dative specified twice"

                if verb_tenses <> [] then
                    failwith "Dative must be specified before quizzes"

                is_dative <- true
            | "pp" ->
                past_participle <- KnownValue(Vocab.FromString(String.concat " " remaining_tags))
                remaining_tags <- []
            | _ -> failwithf "Unrecognised tag '%s' for verb: %O" next t.Vocab

        while remaining_tags <> [] do
            let next = remaining_tags.Head
            remaining_tags <- remaining_tags.Tail
            read_tag(next)

        {
            Infinitive = t.Vocab
            PastParticiple = past_participle
            Tenses = verb_tenses |> List.distinct
            Dative = is_dative
        }

    static member FromString(line: string) : Verb =
        Verb.FromTaggedVocab(TaggedVocab.FromString(line))

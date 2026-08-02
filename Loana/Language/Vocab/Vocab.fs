namespace Loana.Language

open System

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

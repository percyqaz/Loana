namespace Loana.Language

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

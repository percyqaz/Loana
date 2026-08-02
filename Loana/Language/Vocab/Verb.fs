namespace Loana.Language

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

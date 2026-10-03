namespace Loana.Language

type ImperativePerson =
    | ThirdPluralFormal
    | SecondSingular
    | SecondPlural

    override this.ToString() : string =
        match this with
        | ThirdPluralFormal -> "F3p"
        | SecondSingular -> "2"
        | SecondPlural -> "2p"

    static member FromString(value: string) : ImperativePerson =
        match value with
        | "F3p" -> ThirdPluralFormal
        | "2" -> SecondSingular
        | "2p" -> SecondPlural
        | _ -> failwithf "Unrecognised imperative person '%s'" value

type ConjugationPerson =
    | FirstSingular
    | FirstPlural
    | ThirdPlural_Formal
    | SecondSingular
    | SecondPlural
    | ThirdSingular

    override this.ToString() : string =
        match this with
        | FirstSingular -> "1"
        | FirstPlural -> "1p"
        | ThirdPlural_Formal -> "F3p"
        | SecondSingular -> "2"
        | SecondPlural -> "2p"
        | ThirdSingular -> "3"

    member this.ReflexivePronoun(is_dative: bool) : string =
        match this, is_dative with
        | ConjugationPerson.FirstSingular, false -> "mich"
        | ConjugationPerson.FirstSingular, true -> "mir"
        | ConjugationPerson.FirstPlural, _ -> "uns"
        | ConjugationPerson.SecondSingular, false -> "dich"
        | ConjugationPerson.SecondSingular, true -> "dir"
        | ConjugationPerson.ThirdSingular, _ -> "sich"
        | ConjugationPerson.ThirdPlural_Formal, _ -> "sich"
        | ConjugationPerson.SecondPlural, _ -> "euch"

    static member FromString(value: string) : ConjugationPerson =
        match value with
        | "1" -> FirstSingular
        | "1p" -> FirstPlural
        | "F3p" -> ThirdPlural_Formal
        | "2" -> SecondSingular
        | "2p" -> SecondPlural
        | "3" -> ThirdSingular
        | _ -> failwithf "Unrecognised tense person '%s'" value

    static member FromPerson(person: Person) : ConjugationPerson =
        match person with
        | Person.First false -> FirstSingular
        | Person.First true -> FirstPlural
        | Person.Third Gender.Plural
        | Person.Formal -> ThirdPlural_Formal
        | Person.Second false -> SecondSingular
        | Person.Second true -> SecondPlural
        | Person.Third _ -> ThirdSingular

    static member val LIST: ConjugationPerson list =
        [
            FirstSingular
            FirstPlural
            ThirdPlural_Formal
            SecondSingular
            SecondPlural
            ThirdSingular
        ]

type Conjugation =
    | Present of ConjugationPerson
    | SimplePast of ConjugationPerson
    | Imperative of ImperativePerson

    member this.ToTense: VerbTense =
        match this with
        | Present _ -> VerbTense.Present
        | SimplePast _ -> VerbTense.SimplePast
        | Imperative _ -> VerbTense.Imperative

    override this.ToString() : string =
        match this with
        | Present person -> sprintf "pr/%O" person
        | SimplePast person -> sprintf "pa/%O" person
        | Imperative person -> sprintf "im/%O" person

    static member FromString(value: string) : Conjugation =
        if value.StartsWith("pr/") then ConjugationPerson.FromString(value.Substring(3)) |> Present
        elif value.StartsWith("pa/") then ConjugationPerson.FromString(value.Substring(3)) |> SimplePast
        elif value.StartsWith("im/") then ImperativePerson.FromString(value.Substring(3)) |> Imperative
        else failwithf "Unrecognised verb conjugation '%s'" value

type ConjugationTense = Map<ConjugationPerson, string>
type ConjugationImperative = Map<ImperativePerson, string>

type ConjugationContext =
    {
        ParsedVerb: ParsedVerb
        Present: ConjugationTense
        SimplePast: ConjugationTense
        Imperative: ConjugationImperative
    }

    member this.Conjugate(conjugation: Conjugation) : string =

        let inline reflexive_pronoun (person: ConjugationPerson) =
            match this.ParsedVerb.ReflexivePronoun with
            | Some Case.Dative -> " " + person.ReflexivePronoun(true)
            | Some Case.Accusative -> " " + person.ReflexivePronoun(false)
            | _ -> ""

        let conjugated, reflexive_pronoun =
            match conjugation with
            | Conjugation.Present person -> this.Present.[person], reflexive_pronoun(person)
            | Conjugation.SimplePast person -> this.SimplePast.[person], reflexive_pronoun(person)
            | Conjugation.Imperative person -> failwithf "nyi %O" person

        let inline spaced (text: string option) =
            text |> Option.map(fun s -> " " + s) |> Option.defaultValue ""

        conjugated + reflexive_pronoun + spaced this.ParsedVerb.Objects + spaced this.ParsedVerb.SeparableParticle

    member this.ConjugateAll(verb: Verb) : (Conjugation * string) seq =
        seq {
            for q in verb.Tenses do
                match q with
                | VerbTense.Present ->
                    for person in ConjugationPerson.LIST do
                        let c = Conjugation.Present(person)
                        yield c, this.Conjugate(c)
                | VerbTense.SimplePast ->
                    for person in ConjugationPerson.LIST do
                        let c = Conjugation.SimplePast(person)
                        yield c, this.Conjugate(c)
                | VerbTense.Imperative -> failwith "nyi"
        }

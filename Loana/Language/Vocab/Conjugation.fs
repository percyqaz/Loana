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

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

type InflectionPerson =
    | FirstSingular
    | FirstThirdPluralFormal
    | SecondSingular
    | SecondPlural
    | ThirdSingular

    override this.ToString() : string =
        match this with
        | FirstSingular -> "1"
        | FirstThirdPluralFormal -> "F1p3p"
        | SecondSingular -> "2"
        | SecondPlural -> "2p"
        | ThirdSingular -> "3"

    static member FromString(value: string) : InflectionPerson =
        match value with
        | "1" -> FirstSingular
        | "F1p3p" -> FirstThirdPluralFormal
        | "2" -> SecondSingular
        | "2p" -> SecondPlural
        | "3" -> ThirdSingular
        | _ -> failwithf "Unrecognised tense person '%s'" value

    static member FromPerson(person: Person) : InflectionPerson =
        match person with
        | Person.First false -> FirstSingular
        | Person.First true
        | Person.Third Gender.Plural
        | Person.Formal -> FirstThirdPluralFormal
        | Person.Second false -> SecondSingular
        | Person.Second true -> SecondPlural
        | Person.Third _ -> ThirdSingular

type VerbInflection =
    | Present of InflectionPerson
    | SimplePast of InflectionPerson
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

    static member FromString(value: string) : VerbInflection =
        if value.StartsWith("pr/") then InflectionPerson.FromString(value.Substring(3)) |> Present
        elif value.StartsWith("pa/") then InflectionPerson.FromString(value.Substring(3)) |> SimplePast
        elif value.StartsWith("im/") then ImperativePerson.FromString(value.Substring(3)) |> Imperative
        else failwithf "Unrecognised verb inflection '%s'" value

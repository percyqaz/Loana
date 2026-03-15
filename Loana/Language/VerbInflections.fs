namespace Loana.Language

type ImperativePerson =
    | ThirdPluralFormal
    | SecondSingular
    | SecondPlural
        
    override this.ToString() =
        match this with
        | ThirdPluralFormal -> "F3p"
        | SecondSingular -> "2"
        | SecondPlural -> "2p"
        
    static member Parse(value: string) : ImperativePerson =
        match value with
        | "F3p" -> ThirdPluralFormal
        | "2" -> SecondSingular
        | "2p" -> SecondPlural
        | _ -> failwithf "Unrecognised imperative person '%s'" value
        
type TensePerson =
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
        
    override this.ToString() =
        match this with
        | FirstSingular -> "1"
        | FirstThirdPluralFormal -> "F1p3p"
        | SecondSingular -> "2"
        | SecondPlural -> "2p"
        | ThirdSingular -> "3"
        
    static member Parse(value: string) =
        match value with
        | "1" -> FirstSingular
        | "F1p3p" -> FirstThirdPluralFormal
        | "2" -> SecondSingular
        | "2p" -> SecondPlural
        | "3" -> ThirdSingular
        | _ -> failwithf "Unrecognised tense person '%s'" value
        
type VerbInflection =
    | Present of TensePerson
    | SimplePast of TensePerson
    | Imperative of ImperativePerson
    
    member this.AsQuiz =
        match this with
        | Present _ -> VerbQuiz.Present
        | SimplePast _ -> VerbQuiz.SimplePast
        | Imperative _ -> VerbQuiz.Imperative

    override this.ToString() =
        match this with
        | Present p -> sprintf "pr/%O" p
        | SimplePast p -> sprintf "pa/%O" p
        | Imperative p -> sprintf "im/%O" p
        
    static member Parse(value: string) =
        if value.StartsWith("pr/") then
            TensePerson.Parse(value.Substring(3)) |> Present
        elif value.StartsWith("pa/") then
            TensePerson.Parse(value.Substring(3)) |> SimplePast
        elif value.StartsWith("im/") then
            ImperativePerson.Parse(value.Substring(3)) |> Imperative
        else
            failwithf "Unrecognised verb inflection '%s'" value
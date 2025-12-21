namespace Loana

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

[<RequireQualifiedAccess>]
type Case =
    | Nominative
    | Accusative
    | Dative
    | Genitive
    member this.Shorthand =
        match this with
        | Nominative -> "Nom."
        | Accusative -> "Acc."
        | Dative -> "Dat."
        | Genitive -> "Gen."

[<RequireQualifiedAccess>]
type Person =
    | First of plural: bool
    | Second of plural: bool
    | Third of Gender
    | Formal

type SingularNounGuts = {
    Plural: string option
}

type PluralNounGuts = {
    Singular: string option
}

type NounGuts =
    | Masculine of SingularNounGuts
    | Feminine of SingularNounGuts
    | Neuter of SingularNounGuts
    | Plural of PluralNounGuts
    member this.Gender =
        match this with
        | Masculine _ -> Gender.Masculine
        | Feminine _ -> Gender.Feminine
        | Neuter _ -> Gender.Neuter
        | Plural _ -> Gender.Plural

type Noun = {
    Deutsch: string
    English: string
    Guts: NounGuts
}

type Adjective = {
    Deutsch: string
    English: string
}
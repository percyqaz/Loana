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
        | Nominative -> "nom"
        | Accusative -> "acc"
        | Dative -> "dat"
        | Genitive -> "gen"

[<RequireQualifiedAccess>]
type Person =
    | First of plural: bool
    | Second of plural: bool
    | Third of Gender
    | Formal
    member this.Shorthand =
        match this with
        | First false -> "1"
        | First true -> "1p"
        | Second false -> "2"
        | Second true -> "2p"
        | Third g -> "3" + g.ToString()
        | Formal -> "F"

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

type Noun =
    {
        Deutsch: string
        English: string
        Guts: NounGuts
    }
    member this.Key =
        this.Deutsch
            .ToLowerInvariant()
            .Replace("ö", "oe")
            .Replace("ä", "ae")
            .Replace("ü", "ue")
            .Replace("ß", "ss")
            .Replace("-", "_")

type Adjective =
    {
        Deutsch: string
        English: string
    }
    member this.Key =
        this.Deutsch
            .ToLowerInvariant()
            .Replace("ö", "oe")
            .Replace("ä", "ae")
            .Replace("ü", "ue")
            .Replace("ß", "ss")
            .Replace("-", "_")
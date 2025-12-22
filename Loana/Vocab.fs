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

    override this.ToString() =
        match this with
        | Nominative -> "nom"
        | Accusative -> "acc"
        | Dative -> "dat"
        | Genitive -> "gen"

    static member LIST =
        [
            Nominative
            Accusative
            Dative
            Genitive
        ]

[<RequireQualifiedAccess>]
type Person =
    | First of plural: bool
    | Second of plural: bool
    | Third of Gender
    | Formal

    override this.ToString() =
        match this with
        | First false -> "1"
        | First true -> "1p"
        | Second false -> "2"
        | Second true -> "2p"
        | Third g -> "3" + g.ToString()
        | Formal -> "F"

    static member LIST =
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

    override this.ToString() =
        this.Guts.Gender.ToString() + "_" +
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

    override this.ToString() =
        this.Deutsch
            .ToLowerInvariant()
            .Replace("ö", "oe")
            .Replace("ä", "ae")
            .Replace("ü", "ue")
            .Replace("ß", "ss")
            .Replace("-", "_")
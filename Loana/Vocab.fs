namespace Loana

open Avalonia.Media

module Key =

    let of_german (text: string) =
        text
            .ToLowerInvariant()
            .Replace("ö", "oe")
            .Replace("ä", "ae")
            .Replace("ü", "ue")
            .Replace("ß", "ss")
            .Replace("-", "_")
            .Replace(" ", "_")

type Knowledge<'T> =
    | ToBeDetermined
    | Nothing
    | Something of 'T

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

    member this.Color : IBrush =
        match this with
        | Masculine -> Brushes.Blue
        | Neuter -> Brushes.Gray
        | Feminine -> Brushes.Magenta
        | Plural -> Brushes.Yellow

    static member Parse(string: string) : Gender =
        match string with
        | "m" -> Masculine
        | "f" -> Feminine
        | "n" -> Neuter
        | "p" -> Plural
        | _ -> failwithf "could not parse gender from '%s'" string

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

    member this.Color : IBrush =
        match this with
        | Nominative -> Brushes.Green
        | Accusative -> Brushes.Cyan
        | Dative -> Brushes.DarkMagenta
        | Genitive -> Brushes.Gold

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

type Translation =
    {
        Deutsch: string
        English: string
        EnglishAlternatives: string list
    }

type NounGuts =
    | Masculine of plural: Knowledge<Translation>
    | Feminine of plural: Knowledge<Translation>
    | Neuter of plural: Knowledge<Translation>
    | Plural

    member this.Gender =
        match this with
        | Masculine _ -> Gender.Masculine
        | Feminine _ -> Gender.Feminine
        | Neuter _ -> Gender.Neuter
        | Plural -> Gender.Plural

type Noun =
    {
        Translation: Translation
        Guts: NounGuts
    }

    member this.Deutsch = this.Translation.Deutsch
    member this.English = this.Translation.English
    member this.EnglishAlternatives = this.Translation.EnglishAlternatives

    member this.PluralForm : Noun option =
        match this.Guts with
        | Plural -> Some this
        | Masculine (Something plural)
        | Feminine (Something plural)
        | Neuter (Something plural) -> Some { Translation = plural; Guts = Plural }
        | _ -> None

    override this.ToString() =
        this.Guts.Gender.ToString() + "_" + Key.of_german this.Deutsch

type Adjective =
    {
        Translation: Translation
    }

    member this.Deutsch = this.Translation.Deutsch
    member this.English = this.Translation.English
    member this.EnglishAlternatives = this.Translation.EnglishAlternatives

    override this.ToString() = Key.of_german this.Deutsch

type VerbPerson =
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

type VerbInflection =
    | Present of VerbPerson
    | SimplePast of VerbPerson
    | PastParticiple
    | Imperative

[<RequireQualifiedAccess>]
type VerbTag =
    | None
    | Intransitive
    | Transitive
    | Reflexive
    | Reciprocal

type Verb =
    {
        Infinitive: Translation
        Tag: VerbTag
        Separable: bool
        Inflections: Map<VerbInflection, (string * string) option>
    }

    member this.Deutsch = this.Infinitive.Deutsch
    member this.English = this.Infinitive.English
    member this.EnglishAlternatives = this.Infinitive.EnglishAlternatives

    member this.Inflection(inflection: VerbInflection) =
        match Map.tryFind inflection this.Inflections with
        | Some (None) -> Nothing
        | Some (Some (de, en)) -> Something {| Deutsch = de; English = en |}
        | None -> ToBeDetermined

    member this.WithInflection(inflection: VerbInflection, de: string, en: string) =
        { this with Inflections = this.Inflections.Add(inflection, Some (de, en)) }

    member this.WithoutInflection(inflection: VerbInflection) =
        { this with Inflections = this.Inflections.Remove inflection }

    static member Regular(infinitive_de: string, infinitive_en: string) =
        let stem =
            if infinitive_de.EndsWith "en" then
                infinitive_de.Substring(0, infinitive_de.Length - 2)
            elif infinitive_de.EndsWith "eln" then
                infinitive_de.Substring(0, infinitive_de.Length - 1)
            elif infinitive_de.EndsWith "ern" then
                infinitive_de.Substring(0, infinitive_de.Length - 1)
            else
                failwithf "Don't know what to do with this verb '%s' if regular? Maybe typo" infinitive_de

        {
            Infinitive = { Deutsch = infinitive_de; English = infinitive_en; EnglishAlternatives = [] }
            Tag = VerbTag.None
            Separable = false
            Inflections = Map.empty
        }
            .WithInflection(PastParticiple, "ge" + stem + "t", infinitive_en + "ed")
            .WithInflection(Present FirstSingular, stem + "e", infinitive_en)
            .WithInflection(Present FirstThirdPluralFormal, stem + "en", infinitive_en)
            .WithInflection(Present SecondSingular, stem + "st", infinitive_en)
            .WithInflection(Present SecondPlural, stem + "t", infinitive_en)
            .WithInflection(Present ThirdSingular, stem + "t", infinitive_en + "s")
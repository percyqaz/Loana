namespace Loana.Language

open System

type ParsedVerb =
    {
        ReflexivePronoun: Case option
        Objects: string option
        SeparableParticle: string option
        Base: string
    }

    static member FromVerb(verb: Verb) : ParsedVerb =
        let verb_base = verb.Infinitive.Deutsch.RemoveCaseAnnotations()

        let reflexive_pronoun, verb_base =
            if verb_base.StartsWith("sich+D ") then Some Case.Dative, verb_base.Substring(7)
            elif verb_base.StartsWith("sich+A") then Some Case.Accusative, verb_base.Substring(7)
            elif verb_base.StartsWith("sich ") then Some Case.Accusative, verb_base.Substring(5)
            else None, verb_base

        let verb_objects, verb_base =
            let words =
                verb_base
                    .RemoveCaseAnnotations()
                    .Split(' ', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

            if words.Length > 1 then
                Array.truncate (words.Length - 1) words |> String.concat " " |> Some, words.[words.Length - 1]
            else
                None, words.[words.Length - 1]

        let separated_particle, verb_base =
            if verb_base.Contains(".") then
                let s = verb_base.Split(".", 2)
                Some s.[0], s.[1]
            else
                None, verb_base

        {
            ReflexivePronoun = reflexive_pronoun
            Objects = verb_objects
            SeparableParticle = separated_particle
            Base = verb_base
        }

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
            | Some Case.Dative -> " " + Deutsch.reflexive_pronoun_conjugation_person person true
            | Some Case.Accusative -> " " + Deutsch.reflexive_pronoun_conjugation_person person false
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

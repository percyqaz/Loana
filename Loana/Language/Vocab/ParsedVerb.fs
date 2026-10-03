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
        let verb_base = verb.Infinitive.Deutsch

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

namespace Loana.Verbs

open Loana.Language

type VerbCard =

    static member Inflection(v: Verb, i: VerbInflection, inflected_text: string) : Card =
        {
            Key = $"verb-{v.Infinitive.DeutschAsciiIdentifier}-{i.ToString()}"
            Type = Inflection(v, i, inflected_text)
            Tier = 1
            ReferenceKey = v.Infinitive.DeutschAsciiIdentifier
            BumpKey = None
        }

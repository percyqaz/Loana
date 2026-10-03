namespace Loana.Verbs

open Loana.Language

type VerbCard =

    static member ConjugateVerb(v: Verb, c: Conjugation, conjugated_text: string) : Card =
        {
            Key = $"verb-{v.Infinitive.DeutschAsciiIdentifier}-{c.ToString()}"
            Type = ConjugateVerb(v, c, conjugated_text)
            Tier = 1
            ReferenceKey = v.Infinitive.DeutschAsciiIdentifier
            BumpKey = None
        }

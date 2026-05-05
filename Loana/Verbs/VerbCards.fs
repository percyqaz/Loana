namespace Loana.Verbs

open Loana.CLI
open Loana.Language

type VerbCard =

    static member M_Inflection(v: Verb, i: VerbInflection, inflected_text: string) : CardMeta =
        {
            Key = $"verb-{v.Infinitive.Key}-{i.ToString()}"
            Type = Inflection(v, i, inflected_text)
            Tier = 1
            ReferenceKey = v.Infinitive.Key
            BumpKey = None
        }
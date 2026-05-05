namespace Loana.CLI

open Loana.Language

type CardType =
    | RecogniseDE of v: Vocab
    | RecallDE of v: Vocab
    | RecogniseArticleDE of n: Noun
    | RecallArticleDE of n: Noun
    | Inflection of v: Verb * i: VerbInflection * inflected_text: string

type CardMeta =
    {
        Key: string
        Type: CardType
        ReferenceKey: string
        Tier: int
        BumpKey: string option
    }
namespace Loana.Language

type CardType =
    | RecogniseDE of vocab: Vocab
    | RecallDE of vocab: Vocab
    | RecogniseArticleDE of noun: Noun
    | RecallArticleDE of noun: Noun
    | ConjugateVerb of verb: Verb * conjugation: Conjugation * conjugated_text: string

type Card =
    {
        Key: string
        Type: CardType
        ReferenceKey: string
        Tier: int
        BumpKey: string option
    }

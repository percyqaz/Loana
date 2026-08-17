namespace Loana.Vocab

open Loana.Language

type VocabCard =

    static member Tier1RecogniseDE(v: Vocab) : Card =
        {
            Key = $"vocab-recognise-{v.DeutschAsciiIdentifier}"
            Type = RecogniseDE v
            Tier = 1
            ReferenceKey = v.DeutschAsciiIdentifier
            BumpKey = None
        }

    static member Tier2RecallDE(v: Vocab) : Card =
        {
            Key = $"vocab-recall-{v.DeutschAsciiIdentifier}"
            Type = RecallDE v
            Tier = 2
            ReferenceKey = v.DeutschAsciiIdentifier
            BumpKey = Some $"vocab-recognise-{v.DeutschAsciiIdentifier}"
        }

    static member Tier1RecogniseArticleDE(n: Noun) : Card =
        {
            Key = $"noun-recognise-{n.AsciiIdentifierWithGender}"
            Type = RecogniseArticleDE n
            Tier = 1
            ReferenceKey = n.Translation.DeutschAsciiIdentifier
            BumpKey = None
        }

    static member Tier2RecallArticleDE(n: Noun) : Card =
        {
            Key = $"noun-recall-{n.AsciiIdentifierWithGender}"
            Type = RecallArticleDE n
            Tier = 2
            ReferenceKey = n.Translation.DeutschAsciiIdentifier
            BumpKey = Some $"noun-recognise-{n.AsciiIdentifierWithGender}"
        }

    static member Tier3RecognisePluralDE(n: Noun) : Card =
        assert n.Guts.IsPlural
        { VocabCard.Tier1RecogniseArticleDE(n) with Tier = 3 }

    static member Tier4RecallPluralDE(n: Noun) : Card =
        assert n.Guts.IsPlural
        { VocabCard.Tier2RecallArticleDE(n) with Tier = 4 }

    static member Tier3RecognisePastParticipleDE(v: Vocab) : Card =
        { VocabCard.Tier1RecogniseDE(v) with Tier = 3 }

    static member Tier4RecallPastParticipleDE(v: Vocab) : Card =
        { VocabCard.Tier2RecallDE(v) with Tier = 4 }

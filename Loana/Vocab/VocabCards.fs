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

    static member Tier3RecogniseArticleDE(n: Noun) : Card =
        {
            Key = $"noun-recognise-{n.AsciiIdentifierWithGender}"
            Type = RecogniseArticleDE n
            Tier = 3
            ReferenceKey = n.Translation.DeutschAsciiIdentifier
            BumpKey = None
        }

    static member Tier4RecallArticleDE(n: Noun) : Card =
        {
            Key = $"noun-recall-{n.AsciiIdentifierWithGender}"
            Type = RecallArticleDE n
            Tier = 4
            ReferenceKey = n.Translation.DeutschAsciiIdentifier
            BumpKey = Some $"noun-recognise-{n.AsciiIdentifierWithGender}"
        }

    static member Tier5RecognisePluralDE(n: Noun) : Card =
        assert n.Guts.IsPlural
        { VocabCard.Tier3RecogniseArticleDE(n) with Tier = 5 }

    static member Tier6RecallPluralDE(n: Noun) : Card =
        assert n.Guts.IsPlural
        { VocabCard.Tier4RecallArticleDE(n) with Tier = 6 }

    static member Tier3RecognisePastParticipleDE(v: Vocab) : Card =
        { VocabCard.Tier1RecogniseDE(v) with Tier = 3 }

    static member Tier4RecallPastParticipleDE(v: Vocab) : Card =
        { VocabCard.Tier2RecallDE(v) with Tier = 4 }

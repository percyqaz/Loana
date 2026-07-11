namespace Loana.Vocab

open Loana.Language

type VocabCard =

    static member M_Tier1_RecogniseDE(v: Vocab) =
        {
            Key = $"vocab-recognise-{v.DeutschAsciiIdentifier}"
            Type = RecogniseDE v
            Tier = 1
            ReferenceKey = v.DeutschAsciiIdentifier
            BumpKey = None
        }

    static member M_Tier2_RecallDE(v: Vocab) =
        {
            Key = $"vocab-recall-{v.DeutschAsciiIdentifier}"
            Type = RecallDE v
            Tier = 2
            ReferenceKey = v.DeutschAsciiIdentifier
            BumpKey = Some $"vocab-recognise-{v.DeutschAsciiIdentifier}"
        }

    static member M_Tier3_RecogniseArticleDE(n: Noun) =
        {
            Key = $"noun-recognise-{n.AsciiIdentifierWithGender}"
            Type = RecogniseArticleDE n
            Tier = 3
            ReferenceKey = n.Translation.DeutschAsciiIdentifier
            BumpKey = None
        }

    static member M_Tier4_RecallArticleDE(n: Noun) =
        {
            Key = $"noun-recall-{n.AsciiIdentifierWithGender}"
            Type = RecallArticleDE n
            Tier = 4
            ReferenceKey = n.Translation.DeutschAsciiIdentifier
            BumpKey = Some $"noun-recognise-{n.AsciiIdentifierWithGender}"
        }

    static member M_Tier5_RecognisePluralDE(n: Noun) =
        assert n.Guts.IsPlural
        { VocabCard.M_Tier3_RecogniseArticleDE(n) with Tier = 5 }

    static member M_Tier6_RecallPluralDE(n: Noun) =
        assert n.Guts.IsPlural
        { VocabCard.M_Tier4_RecallArticleDE(n) with Tier = 6 }

    static member M_Tier3_RecognisePastParticipleDE(v: Vocab) =
        { VocabCard.M_Tier1_RecogniseDE(v) with Tier = 3 }

    static member M_Tier4_RecallPastParticipleDE(v: Vocab) =
        { VocabCard.M_Tier2_RecallDE(v) with Tier = 4 }

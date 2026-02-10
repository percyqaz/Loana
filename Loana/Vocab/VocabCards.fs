namespace Loana.Vocab

open System.Drawing
open Loana.Language
open Loana.CLI

type VocabCard =

    static let GERMAN_BG = Color.FromArgb(0x400000)
    static let ENGLISH_NOTE = Color.FromArgb(0x808080)
    static let GERMAN_NOTE = Color.FromArgb(0xC0C0C0)

    static member M_Tier1_RecogniseDE(v: Vocab) =
        {
            Key = $"vocab-recognise-{v.Key}"
            Tier = 1
            ReferenceKey = v.Key
            BumpKey = None
        }
    static member C_Tier1_RecogniseDE(v: Vocab) =
        let en_side =
            seq {
                yield { Text = v.English.Text; FG = Color.Black; BG = Color.White }
                match v.English.Note with
                | Some n -> yield { Text = $" [{n}]"; FG = ENGLISH_NOTE; BG = Color.White }
                | None -> ()
                for alt in v.EnglishAlternatives do
                    yield { Text = ", "; FG = Color.Black; BG = Color.White }
                    yield { Text = alt.Text; FG = Color.Black; BG = Color.White }
                    match alt.Note with
                    | Some n -> yield { Text = $" [{n}]"; FG = ENGLISH_NOTE; BG = Color.White }
                    | None -> ()
            }
            |> List.ofSeq
            |> CardLine.Create Color.White
        {
            Meta = VocabCard.M_Tier1_RecogniseDE(v: Vocab)
            Front = fun () ->
                CardSide.Create [
                    CardLine.Create GERMAN_BG []
                    CardLine.Create GERMAN_BG [ { Text = v.Deutsch; FG = Color.White; BG = GERMAN_BG } ]
                    CardLine.Create GERMAN_BG []
                    CardLine.Create Color.White []
                    CardLine.Create Color.White [ { Text = "???"; FG = Color.Black; BG = Color.White } ]
                    CardLine.Create Color.White []
                ]
            Back = fun () ->
                CardSide.Create [
                    CardLine.Create GERMAN_BG []
                    CardLine.Create GERMAN_BG [ { Text = v.Deutsch; FG = Color.White; BG = GERMAN_BG } ]
                    CardLine.Create GERMAN_BG []
                    CardLine.Create Color.White []
                    en_side
                    CardLine.Create Color.White []
                ]
        }

    static member M_Tier2_RecallDE(v: Vocab) =
        {
            Key = $"vocab-recall-{v.Key}"
            Tier = 2
            ReferenceKey = v.Key
            BumpKey = Some $"vocab-recognise-{v.Key}"
        }
    static member C_Tier2_RecallDE(v: Vocab) =
        let en_side =
            seq {
                yield { Text = v.English.Text; FG = Color.Black; BG = Color.White }
                match v.English.Note with
                | Some n -> yield { Text = $" [{n}]"; FG = ENGLISH_NOTE; BG = Color.White }
                | None -> ()
                for alt in v.EnglishAlternatives do
                    yield { Text = ", "; FG = Color.Black; BG = Color.White }
                    yield { Text = alt.Text; FG = Color.Black; BG = Color.White }
                    match alt.Note with
                    | Some n -> yield { Text = $" [{n}]"; FG = ENGLISH_NOTE; BG = Color.White }
                    | None -> ()
            }
            |> List.ofSeq
            |> CardLine.Create Color.White
        {
            Meta = VocabCard.M_Tier2_RecallDE(v)
            Front = fun () ->
                CardSide.Create [
                    CardLine.Create Color.White []
                    en_side
                    CardLine.Create Color.White []
                    CardLine.Create GERMAN_BG []
                    CardLine.Create GERMAN_BG [ { Text = "???"; FG = Color.White; BG = GERMAN_BG } ]
                    CardLine.Create GERMAN_BG []
                ]
            Back = fun () ->
                CardSide.Create [
                    CardLine.Create Color.White []
                    en_side
                    CardLine.Create Color.White []
                    CardLine.Create GERMAN_BG []
                    CardLine.Create GERMAN_BG [ { Text = v.Deutsch; FG = Color.White; BG = GERMAN_BG } ]
                    CardLine.Create GERMAN_BG []
                ]
        }

    static member M_Tier3_RecogniseArticleDE(n: Noun) =
        {
            Key = $"noun-recognise-{n.KeyWithGender}"
            Tier = 3
            ReferenceKey = n.Translation.Key
            BumpKey = None
        }
    static member C_Tier3_RecogniseArticleDE(n: Noun) =
        let en_side =
            seq {
                yield { Text = "the "; FG = ENGLISH_NOTE; BG = Color.White }
                yield { Text = n.English.Text; FG = Color.Black; BG = Color.White }
                match n.English.Note with
                | Some n -> yield { Text = $" [{n}]"; FG = ENGLISH_NOTE; BG = Color.White }
                | None -> ()
                for alt in n.EnglishAlternatives do
                    yield { Text = ", "; FG = Color.Black; BG = Color.White }
                    yield { Text = "the "; FG = ENGLISH_NOTE; BG = Color.White }
                    yield { Text = alt.Text; FG = Color.Black; BG = Color.White }
                    match alt.Note with
                    | Some n -> yield { Text = $" [{n}]"; FG = ENGLISH_NOTE; BG = Color.White }
                    | None -> ()
            }
            |> List.ofSeq
            |> CardLine.Create Color.White
        let de_side_white, de_side_colored =
            let article = AnnotationTree.flatten_tree (Deutsch.definite_article n.Guts.Gender Case.Nominative)
            CardLine.Create GERMAN_BG [
                { Text = article + " "; FG = GERMAN_NOTE; BG = GERMAN_BG }
                { Text = n.Deutsch; FG = Color.White; BG = GERMAN_BG }
            ],
            CardLine.Create GERMAN_BG [
                { Text = article + " "; FG = GERMAN_NOTE; BG = GERMAN_BG }
                { Text = n.Deutsch; FG = n.Guts.Gender.Color; BG = GERMAN_BG }
            ]
        {
            Meta = VocabCard.M_Tier3_RecogniseArticleDE(n)
            Front = fun () ->
                CardSide.Create [
                    CardLine.Create GERMAN_BG []
                    de_side_white
                    CardLine.Create GERMAN_BG []
                    CardLine.Create Color.White []
                    CardLine.Create Color.White [ { Text = "???"; FG = Color.Black; BG = Color.White } ]
                    CardLine.Create Color.White []
                ]
            Back = fun () ->
                CardSide.Create [
                    CardLine.Create GERMAN_BG []
                    de_side_colored
                    CardLine.Create GERMAN_BG []
                    CardLine.Create Color.White []
                    en_side
                    CardLine.Create Color.White []
                ]
        }

    static member M_Tier4_RecallArticleDE(n: Noun) =
        {
            Key = $"noun-recall-{n.KeyWithGender}"
            Tier = 4
            ReferenceKey = n.Translation.Key
            BumpKey = Some $"noun-recognise-{n.KeyWithGender}"
        }
    static member C_Tier4_RecallArticleDE(n: Noun) =
        let en_side =
            seq {
                yield { Text = "the "; FG = ENGLISH_NOTE; BG = Color.White }
                yield { Text = n.English.Text; FG = Color.Black; BG = Color.White }
                match n.English.Note with
                | Some n -> yield { Text = $" [{n}]"; FG = ENGLISH_NOTE; BG = Color.White }
                | None -> ()
                for alt in n.EnglishAlternatives do
                    yield { Text = ", "; FG = Color.Black; BG = Color.White }
                    yield { Text = "the "; FG = ENGLISH_NOTE; BG = Color.White }
                    yield { Text = alt.Text; FG = Color.Black; BG = Color.White }
                    match alt.Note with
                    | Some n -> yield { Text = $" [{n}]"; FG = ENGLISH_NOTE; BG = Color.White }
                    | None -> ()
            }
            |> List.ofSeq
            |> CardLine.Create Color.White
        let de_side =
            let article = AnnotationTree.flatten_tree (Deutsch.definite_article n.Guts.Gender Case.Nominative)
            CardLine.Create GERMAN_BG [
                { Text = article + " "; FG = GERMAN_NOTE; BG = GERMAN_BG }
                { Text = n.Deutsch; FG = n.Guts.Gender.Color; BG = GERMAN_BG }
            ]
        {
            Meta = VocabCard.M_Tier4_RecallArticleDE(n)
            Front = fun () ->
                CardSide.Create [
                    CardLine.Create Color.White []
                    en_side
                    CardLine.Create Color.White []
                    CardLine.Create GERMAN_BG []
                    CardLine.Create GERMAN_BG [ { Text = "???"; FG = Color.White; BG = GERMAN_BG } ]
                    CardLine.Create GERMAN_BG []
                ]
            Back = fun () ->
                CardSide.Create [
                    CardLine.Create Color.White []
                    en_side
                    CardLine.Create Color.White []
                    CardLine.Create GERMAN_BG []
                    de_side
                    CardLine.Create GERMAN_BG []
                ]
        }

    static member M_Tier5_RecognisePluralDE(n: Noun) =
        assert(n.Guts.IsPlural)
        { VocabCard.M_Tier3_RecogniseArticleDE(n) with Tier = 5 }
    static member C_Tier5_RecognisePluralDE(n: Noun) =
        assert(n.Guts.IsPlural)
        { VocabCard.C_Tier3_RecogniseArticleDE(n) with Meta = VocabCard.M_Tier5_RecognisePluralDE(n) }

    static member M_Tier6_RecallPluralDE(n: Noun) =
        assert(n.Guts.IsPlural)
        { VocabCard.M_Tier4_RecallArticleDE(n) with Tier = 6 }
    static member C_Tier6_RecallPluralDE(n: Noun) =
        assert(n.Guts.IsPlural)
        { VocabCard.C_Tier4_RecallArticleDE(n) with Meta = VocabCard.M_Tier6_RecallPluralDE(n) }
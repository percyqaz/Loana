namespace Loana.Verbs

open System.Drawing
open Loana.CLI
open Loana.Language

type VerbCard =

    static let QUESTION_BG = Color.FromArgb(0x404040)
    static let PRESENT_BG = Color.FromArgb(0xAAFFAA)
    static let SIMPLE_PAST_BG = Color.FromArgb(0xAAAAFF)
    static let IMPERATIVE_BG = Color.FromArgb(0xFFFFAA)
    static let QUESTION_NOTE = Color.FromArgb(0xC0C0C0)
    static let ANSWER_NOTE = Color.FromArgb(0x808080)

    static member M_Inflection(v: Verb, i: VerbInflection) : CardMeta =
        {
            Key = $"verb-{v.Infinitive.Key}-{i.ToString()}"
            Tier = 1
            ReferenceKey = v.Infinitive.Key
            BumpKey = None
        }
    static member C_Inflection(v: Verb, i: VerbInflection, inflected_text: string) =
        let en_side =
            seq {
                yield { Text = v.Infinitive.English.Text; FG = Color.White; BG = QUESTION_BG }
                match v.Infinitive.English.Note with
                | Some n -> yield { Text = $" [{n}]"; FG = QUESTION_NOTE; BG = QUESTION_BG }
                | None -> ()
                for alt in v.Infinitive.EnglishAlternatives do
                    yield { Text = ", "; FG = Color.White; BG = QUESTION_BG }
                    yield { Text = alt.Text; FG = Color.White; BG = QUESTION_BG }
                    match alt.Note with
                    | Some n -> yield { Text = $" [{n}]"; FG = QUESTION_NOTE; BG = QUESTION_BG }
                    | None -> ()
            }
            |> List.ofSeq
            |> CardLine.Create QUESTION_BG
        let answer_bg =
            match i with
            | Present _ -> PRESENT_BG
            | SimplePast _ -> SIMPLE_PAST_BG
            | Imperative _ -> IMPERATIVE_BG
        let pronoun =
            match i with
            | Present p
            | SimplePast p ->
                match p with
                | FirstSingular -> "ich"
                | FirstThirdPluralFormal -> List.randomChoice ["wir"; "sie"; "Sie"]
                | SecondSingular -> "du"
                | SecondPlural -> "ihr"
                | ThirdSingular -> List.randomChoice ["er"; "sie"; "es"] // todo: tag verbs that use only es
            | Imperative p ->
                match p with
                | ImperativePerson.SecondPlural -> "(ihr)"
                | ImperativePerson.SecondSingular -> "(du)"
                | ImperativePerson.ThirdPluralFormal -> List.randomChoice ["wir"; "Sie"]

        {
            Meta = VerbCard.M_Inflection(v, i)
            Front = fun () ->
                CardSide.Create [
                    CardLine.Create QUESTION_BG []
                    CardLine.Create QUESTION_BG [ { Text = v.Infinitive.Deutsch; FG = Color.White; BG = QUESTION_BG } ]
                    en_side
                    CardLine.Create QUESTION_BG []
                    CardLine.Create answer_bg []
                    CardLine.Create answer_bg [
                        { Text = (if i.IsImperative then "" else pronoun); FG = ANSWER_NOTE; BG = answer_bg }
                        { Text = " ??? "; FG = Color.Black; BG = answer_bg }
                        { Text = (if i.IsImperative then pronoun else ""); FG = ANSWER_NOTE; BG = answer_bg }
                    ]
                    CardLine.Create answer_bg []
                ]
            Back = fun () ->
                CardSide.Create [
                    CardLine.Create QUESTION_BG []
                    CardLine.Create QUESTION_BG [ { Text = v.Infinitive.Deutsch; FG = Color.White; BG = QUESTION_BG } ]
                    en_side
                    CardLine.Create QUESTION_BG []
                    CardLine.Create answer_bg []
                    CardLine.Create answer_bg [
                        { Text = (if i.IsImperative then "" else pronoun); FG = ANSWER_NOTE; BG = answer_bg }
                        { Text = " " + inflected_text + " "; FG = Color.Black; BG = answer_bg }
                        { Text = (if i.IsImperative then pronoun else ""); FG = ANSWER_NOTE; BG = answer_bg }
                    ]
                    CardLine.Create answer_bg []
                ]
        }
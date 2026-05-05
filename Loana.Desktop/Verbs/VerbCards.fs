namespace Loana.Verbs

open System.Drawing
open Loana.Language
open Loana.Desktop.CLI

type VerbCard =

    static let QUESTION_BG = Color.FromArgb(0xFF_403030)
    static let PRESENT_BG = Color.FromArgb(0xFF_AAFFAA)
    static let SIMPLE_PAST_BG = Color.FromArgb(0xFF_AAAAFF)
    static let IMPERATIVE_BG = Color.FromArgb(0xFF_FFFFAA)
    static let QUESTION_NOTE = Color.FromArgb(0xFF_C0C0C0)
    static let ANSWER_NOTE = Color.FromArgb(0xFF_404040)

    static member Inflection(v: Verb, i: VerbInflection, inflected_text: string) : CardSide * CardSide =
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
        let answer_bg, quiz_hint =
            match i with
            | Present _ -> PRESENT_BG, " [present] "
            | SimplePast _ -> SIMPLE_PAST_BG, " [simple past] "
            | Imperative _ -> IMPERATIVE_BG, " [imperative] "
        let pronoun =
            match i with
            | Present p
            | SimplePast p ->
                match p with
                | FirstSingular -> "ich"
                | FirstThirdPluralFormal -> List.randomChoice ["wir"; "sie [p]"; "Sie"]
                | SecondSingular -> "du"
                | SecondPlural -> "ihr"
                | ThirdSingular -> List.randomChoice ["er"; "sie [f]"; "es"] // todo: tag verbs that use only es
            | Imperative p ->
                match p with
                | ImperativePerson.SecondPlural -> "(ihr)"
                | ImperativePerson.SecondSingular -> "(du)"
                | ImperativePerson.ThirdPluralFormal -> List.randomChoice ["wir"; "Sie"]

        CardSide.Create [
            CardLine.Create QUESTION_BG []
            CardLine.Create QUESTION_BG [ { Text = v.Infinitive.Deutsch; FG = Color.White; BG = QUESTION_BG } ]
            en_side
            CardLine.Create QUESTION_BG []
            CardLine.Create answer_bg []
            CardLine.Create answer_bg [
                { Text = (if i.IsImperative then "" else pronoun); FG = ANSWER_NOTE; BG = answer_bg }
                { Text = quiz_hint; FG = Color.Black; BG = answer_bg }
                { Text = (if i.IsImperative then pronoun else ""); FG = ANSWER_NOTE; BG = answer_bg }
            ]
            CardLine.Create answer_bg []
        ],
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
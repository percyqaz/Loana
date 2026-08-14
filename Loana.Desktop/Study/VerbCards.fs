namespace Loana.Desktop.Verbs

open System.Drawing
open Loana.Desktop.CLI
open Loana.Language
open Loana.Desktop.Study

type VerbCard =

    static let QUESTION_BG = Color.FromArgb(0xFF_403030)
    static let PRESENT_BG = Color.FromArgb(0xFF_AAFFAA)
    static let SIMPLE_PAST_BG = Color.FromArgb(0xFF_AAAAFF)
    static let IMPERATIVE_BG = Color.FromArgb(0xFF_FFFFAA)
    static let QUESTION_NOTE = Color.FromArgb(0xFF_C0C0C0)
    static let ANSWER_NOTE = Color.FromArgb(0xFF_404040)

    static member EnglishVocab(vocab: Vocab, text_color: Color, note_color: Color) : CardLine =
        let mutable line = CardLine.Empty

        let inline write (text: string, format: string -> string) : unit = line <- line.Append(text, format)

        write(vocab.English.Text, _.ForeColor(text_color))

        match vocab.English.Note with
        | Some n -> write($" [{n}]", _.ForeColor(note_color))
        | None -> ()

        for alt in vocab.EnglishAlternatives do
            write(", ", _.ForeColor(text_color))
            write(alt.Text, _.ForeColor(text_color))

            match alt.Note with
            | Some n -> write($" [{n}]", _.ForeColor(note_color))
            | None -> ()

        line

    static member GermanVocab(vocab: Vocab, text_color: Color) : CardLine =
        CardLine.Append(vocab.Deutsch, _.ForeColor(text_color))

    static member RenderInflection(v: Verb, i: VerbInflection, inflected_text: string) : CardSide * CardSide =

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
                | FirstThirdPluralFormal -> List.randomChoice [ "wir"; "Sie" ]
                | SecondSingular -> "du"
                | SecondPlural -> "ihr"
                | ThirdSingular -> List.randomChoice [ "er"; "es" ] // todo: tag verbs that use only es
            | Imperative p ->
                match p with
                | ImperativePerson.SecondPlural -> "(ihr)"
                | ImperativePerson.SecondSingular -> "(du)"
                | ImperativePerson.ThirdPluralFormal -> List.randomChoice [ "wir"; "Sie" ]

        let question_line =
            CardLine
                .Append((if i.IsImperative then "" else pronoun), _.ForeColor(ANSWER_NOTE))
                .Append(quiz_hint, _.ForeColor(Color.Black))
                .Append((if i.IsImperative then pronoun else ""), _.ForeColor(ANSWER_NOTE))

        let reveal_line =
            CardLine
                .Append((if i.IsImperative then "" else pronoun), _.ForeColor(ANSWER_NOTE))
                .Append(" " + inflected_text + " ", _.ForeColor(Color.Black))
                .Append((if i.IsImperative then pronoun else ""), _.ForeColor(ANSWER_NOTE))

        CardSide.Create(
            [
                CardSection.Create(
                    QUESTION_BG,
                    [
                        VerbCard.GermanVocab(v.Infinitive, Color.White)
                        VerbCard.EnglishVocab(v.Infinitive, Color.White, QUESTION_NOTE)
                    ]
                )
                CardSection.Create(answer_bg, question_line)
            ]
        ),
        CardSide.Create(
            [
                CardSection.Create(
                    QUESTION_BG,
                    [
                        VerbCard.GermanVocab(v.Infinitive, Color.White)
                        VerbCard.EnglishVocab(v.Infinitive, Color.White, QUESTION_NOTE)
                    ]
                )
                CardSection.Create(answer_bg, reveal_line)
            ]
        )

namespace Loana.Desktop.Study

open System.Drawing
open Loana.Desktop.CLI
open Loana.Language
open Loana.Desktop.Study

type Cards =

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

    static member EnglishNoun(noun: Noun, text_color: Color, note_color: Color) : CardLine =
        let mutable line = CardLine.Empty

        let inline write (text: string, format: string -> string) : unit = line <- line.Append(text, format)

        write("the ", _.ForeColor(note_color))
        write(noun.Translation.English.Text, _.ForeColor(text_color))

        match noun.Translation.English.Note with
        | Some n -> write($" [{n}]", _.ForeColor(note_color))
        | None -> ()

        for alt in noun.Translation.EnglishAlternatives do
            write(", ", _.ForeColor(text_color))
            write("the ", _.ForeColor(note_color))
            write(alt.Text, _.ForeColor(text_color))

            match alt.Note with
            | Some n -> write($" [{n}]", _.ForeColor(note_color))
            | None -> ()

        line

    static member GermanNounRevealed(noun: Noun, note_color: Color) : CardLine =
        let article =
            AnnotationTree.flatten_tree(Deutsch.definite_article noun.Guts.Gender Case.Nominative)

        CardLine
            .Append(article + " ", _.ForeColor(note_color))
            .Append(noun.Deutsch, _.ForeColor(noun.Guts.Gender.Color))

    static member GermanNoun(noun: Noun, text_color: Color, note_color: Color) : CardLine =
        let article =
            AnnotationTree.flatten_tree(Deutsch.definite_article noun.Guts.Gender Case.Nominative)

        CardLine.Append(article + " ", _.ForeColor(note_color)).Append(noun.Deutsch, _.ForeColor(text_color))

    static let QUESTION_BG = Color.FromArgb(0xFF_403030)
    static let PRESENT_BG = Color.FromArgb(0xFF_AAFFAA)
    static let SIMPLE_PAST_BG = Color.FromArgb(0xFF_AAAAFF)
    static let IMPERATIVE_BG = Color.FromArgb(0xFF_FFFFAA)
    static let QUESTION_NOTE = Color.FromArgb(0xFF_C0C0C0)
    static let ANSWER_NOTE = Color.FromArgb(0xFF_404040)
    static let GERMAN_BG = Color.FromArgb(0xFF_400000)
    static let ENGLISH_NOTE = Color.FromArgb(0xFF_808080)
    static let GERMAN_NOTE = Color.FromArgb(0xFF_C0C0C0)

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
                        Cards.GermanVocab(v.Infinitive, Color.White)
                        Cards.EnglishVocab(v.Infinitive, Color.White, QUESTION_NOTE)
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
                        Cards.GermanVocab(v.Infinitive, Color.White)
                        Cards.EnglishVocab(v.Infinitive, Color.White, QUESTION_NOTE)
                    ]
                )
                CardSection.Create(answer_bg, reveal_line)
            ]
        )

    static member RecogniseDE(v: Vocab) : CardSide * CardSide =
        let de_line = Cards.GermanVocab(v, Color.White)
        let en_line = Cards.EnglishVocab(v, Color.Black, ENGLISH_NOTE)
        let question_line = CardLine.Append("???", _.ForeColor(Color.Black))

        CardSide.Create(
            [
                CardSection.Create(GERMAN_BG, de_line)
                CardSection.Create(Color.White, question_line)
            ]
        ),
        CardSide.Create(
            [
                CardSection.Create(GERMAN_BG, de_line)
                CardSection.Create(Color.White, en_line)
            ]
        )

    static member RecallDE(v: Vocab) : CardSide * CardSide =
        let de_line = Cards.GermanVocab(v, Color.White)
        let en_line = Cards.EnglishVocab(v, Color.Black, ENGLISH_NOTE)
        let question_line = CardLine.Append("???", _.ForeColor(Color.White))

        CardSide.Create(
            [
                CardSection.Create(Color.White, en_line)
                CardSection.Create(GERMAN_BG, question_line)
            ]
        ),
        CardSide.Create(
            [
                CardSection.Create(Color.White, en_line)
                CardSection.Create(GERMAN_BG, de_line)
            ]
        )

    static member RecogniseArticleDE(noun: Noun) : CardSide * CardSide =
        let en_line = Cards.EnglishNoun(noun, Color.Black, ENGLISH_NOTE)
        let de_white = Cards.GermanNoun(noun, Color.White, GERMAN_NOTE)
        let de_revealed = Cards.GermanNounRevealed(noun, GERMAN_NOTE)
        let question_line = CardLine.Append("???", _.ForeColor(Color.Black))

        CardSide.Create(
            [
                CardSection.Create(GERMAN_BG, de_white)
                CardSection.Create(Color.White, question_line)
            ]
        ),
        CardSide.Create(
            [
                CardSection.Create(GERMAN_BG, de_revealed)
                CardSection.Create(Color.White, en_line)
            ]
        )

    static member RecallArticleDE(noun: Noun) : CardSide * CardSide =
        let en_line = Cards.EnglishNoun(noun, Color.Black, ENGLISH_NOTE)
        let de_revealed = Cards.GermanNounRevealed(noun, GERMAN_NOTE)
        let question_line = CardLine.Append("???", _.ForeColor(Color.White))

        CardSide.Create(
            [
                CardSection.Create(Color.White, en_line)
                CardSection.Create(GERMAN_BG, question_line)
            ]
        ),
        CardSide.Create(
            [
                CardSection.Create(Color.White, en_line)
                CardSection.Create(GERMAN_BG, de_revealed)
            ]
        )

    static member Render(card: Card) : CardSide * CardSide =
        match card.Type with
        | RecogniseDE v -> Cards.RecogniseDE(v)
        | RecallDE v -> Cards.RecallDE(v)
        | RecogniseArticleDE n -> Cards.RecogniseArticleDE(n)
        | RecallArticleDE n -> Cards.RecallArticleDE(n)
        | Inflection(verb, inflection, inflected_text) -> Cards.RenderInflection(verb, inflection, inflected_text)

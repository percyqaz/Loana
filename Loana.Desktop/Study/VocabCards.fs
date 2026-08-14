namespace Loana.Desktop.Vocab

open System.Drawing
open Loana.Desktop.CLI
open Loana.Desktop.Verbs
open Loana.Language
open Loana.Desktop.Study

type VocabCard =

    static let GERMAN_BG = Color.FromArgb(0xFF_400000)
    static let ENGLISH_NOTE = Color.FromArgb(0xFF_808080)
    static let GERMAN_NOTE = Color.FromArgb(0xFF_C0C0C0)

    static member RecogniseDE(v: Vocab) : CardSide * CardSide =
        let de_line = VerbCard.GermanVocab(v, Color.White)
        let en_line = VerbCard.EnglishVocab(v, Color.Black, ENGLISH_NOTE)
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
        let de_line = VerbCard.GermanVocab(v, Color.White)
        let en_line = VerbCard.EnglishVocab(v, Color.Black, ENGLISH_NOTE)
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

    static member RecogniseArticleDE(noun: Noun) : CardSide * CardSide =
        let en_line = VocabCard.EnglishNoun(noun, Color.Black, ENGLISH_NOTE)
        let de_white = VocabCard.GermanNoun(noun, Color.White, GERMAN_NOTE)
        let de_revealed = VocabCard.GermanNounRevealed(noun, GERMAN_NOTE)
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
        let en_line = VocabCard.EnglishNoun(noun, Color.Black, ENGLISH_NOTE)
        let de_revealed = VocabCard.GermanNounRevealed(noun, GERMAN_NOTE)
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

    // todo: split into front and back
    static member Render(card: Card) : CardSide * CardSide =
        match card.Type with
        | RecogniseDE v -> VocabCard.RecogniseDE(v)
        | RecallDE v -> VocabCard.RecallDE(v)
        | RecogniseArticleDE n -> VocabCard.RecogniseArticleDE(n)
        | RecallArticleDE n -> VocabCard.RecallArticleDE(n)
        | Inflection(verb, inflection, inflected_text) -> VerbCard.RenderInflection(verb, inflection, inflected_text)

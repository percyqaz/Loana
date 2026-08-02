namespace Loana.Desktop.Vocab

open System.Drawing
open Loana.Language
open Loana.Desktop.CLI

type VocabCard =

    static let GERMAN_BG = Color.FromArgb(0xFF_400000)
    static let ENGLISH_NOTE = Color.FromArgb(0xFF_808080)
    static let GERMAN_NOTE = Color.FromArgb(0xFF_C0C0C0)

    static member RecogniseDE(v: Vocab) : CardSide * CardSide =
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
            |> CardLine.Create(Color.White)

        CardSide.Create(
            [
                CardLine.Create GERMAN_BG []
                CardLine.Create GERMAN_BG [ { Text = v.Deutsch; FG = Color.White; BG = GERMAN_BG } ]
                CardLine.Create GERMAN_BG []
                CardLine.Create Color.White []
                CardLine.Create Color.White [ { Text = "???"; FG = Color.Black; BG = Color.White } ]
                CardLine.Create Color.White []
            ]
        ),
        CardSide.Create(
            [
                CardLine.Create GERMAN_BG []
                CardLine.Create GERMAN_BG [ { Text = v.Deutsch; FG = Color.White; BG = GERMAN_BG } ]
                CardLine.Create GERMAN_BG []
                CardLine.Create Color.White []
                en_side
                CardLine.Create Color.White []
            ]
        )

    static member RecallDE(v: Vocab) : CardSide * CardSide =
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
            |> CardLine.Create(Color.White)

        CardSide.Create(
            [
                CardLine.Create Color.White []
                en_side
                CardLine.Create Color.White []
                CardLine.Create GERMAN_BG []
                CardLine.Create GERMAN_BG [ { Text = "???"; FG = Color.White; BG = GERMAN_BG } ]
                CardLine.Create GERMAN_BG []
            ]
        ),
        CardSide.Create(
            [
                CardLine.Create Color.White []
                en_side
                CardLine.Create Color.White []
                CardLine.Create GERMAN_BG []
                CardLine.Create GERMAN_BG [ { Text = v.Deutsch; FG = Color.White; BG = GERMAN_BG } ]
                CardLine.Create GERMAN_BG []
            ]
        )

    static member RecogniseArticleDE(n: Noun) : CardSide * CardSide =
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
            |> CardLine.Create(Color.White)

        let de_side_white, de_side_colored =
            let article =
                AnnotationTree.flatten_tree(Deutsch.definite_article n.Guts.Gender Case.Nominative)

            CardLine.Create
                GERMAN_BG
                [
                    { Text = article + " "; FG = GERMAN_NOTE; BG = GERMAN_BG }
                    { Text = n.Deutsch; FG = Color.White; BG = GERMAN_BG }
                ],
            CardLine.Create
                GERMAN_BG
                [
                    { Text = article + " "; FG = GERMAN_NOTE; BG = GERMAN_BG }
                    { Text = n.Deutsch; FG = n.Guts.Gender.Color; BG = GERMAN_BG }
                ]

        CardSide.Create(
            [
                CardLine.Create GERMAN_BG []
                de_side_white
                CardLine.Create GERMAN_BG []
                CardLine.Create Color.White []
                CardLine.Create Color.White [ { Text = "???"; FG = Color.Black; BG = Color.White } ]
                CardLine.Create Color.White []
            ]
        ),
        CardSide.Create(
            [
                CardLine.Create GERMAN_BG []
                de_side_colored
                CardLine.Create GERMAN_BG []
                CardLine.Create Color.White []
                en_side
                CardLine.Create Color.White []
            ]
        )

    static member RecallArticleDE(n: Noun) : CardSide * CardSide =
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
            |> CardLine.Create(Color.White)

        let de_side =
            let article =
                AnnotationTree.flatten_tree(Deutsch.definite_article n.Guts.Gender Case.Nominative)

            CardLine.Create
                GERMAN_BG
                [
                    { Text = article + " "; FG = GERMAN_NOTE; BG = GERMAN_BG }
                    { Text = n.Deutsch; FG = n.Guts.Gender.Color; BG = GERMAN_BG }
                ]

        CardSide.Create(
            [
                CardLine.Create Color.White []
                en_side
                CardLine.Create Color.White []
                CardLine.Create GERMAN_BG []
                CardLine.Create GERMAN_BG [ { Text = "???"; FG = Color.White; BG = GERMAN_BG } ]
                CardLine.Create GERMAN_BG []
            ]
        ),
        CardSide.Create(
            [
                CardLine.Create Color.White []
                en_side
                CardLine.Create Color.White []
                CardLine.Create GERMAN_BG []
                de_side
                CardLine.Create GERMAN_BG []
            ]
        )

    static member Render(card: Card) : CardSide * CardSide =
        match card.Type with
        | RecogniseDE v -> VocabCard.RecogniseDE(v)
        | RecallDE v -> VocabCard.RecallDE(v)
        | RecogniseArticleDE n -> VocabCard.RecogniseArticleDE(n)
        | RecallArticleDE n -> VocabCard.RecallArticleDE(n)
        | Inflection _ -> failwith "todo: split verb mode into separate code instead of the hack it is now"

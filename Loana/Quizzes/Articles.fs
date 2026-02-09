namespace Loana.Quizzes

open Loana.Language

module Articles =

    let DEFINITE : Quiz =
        {
            Name = "Definite Articles"
            Key = "quiz-definite-articles"
            Questions = fun () ->
                seq {
                    let adjective = None
                    for noun in NOUNS do
                        for case in Case.LIST do
                            yield GermanPracticeQuestion.Create(
                                English.definite_fragment adjective noun case,
                                Deutsch.definite_fragment adjective noun case
                            )
                }
                |> Seq.randomShuffle
                |> Array.ofSeq
        }

    let INDEFINITE : Quiz =
        {
            Name = "Indefinite Articles"
            Key = "quiz-indefinite-articles"
            Questions = fun () ->
                seq {
                    let adjective = None
                    for noun in NOUNS do
                        for case in Case.LIST do
                            if not noun.Guts.IsPlural then
                                yield GermanPracticeQuestion.Create(
                                    English.indefinite_fragment adjective noun case,
                                    Deutsch.indefinite_fragment adjective noun case
                                )
                }
                |> Seq.randomShuffle
                |> Array.ofSeq
        }

    let MIXED : Quiz =
        {
            Name = "Mixed Articles"
            Key = "quiz-mixed-articles"
            Questions = fun () ->
                seq {
                    let adjective = Some KLEIN
                    for noun in NOUNS do
                        for case in Case.LIST do
                            yield GermanPracticeQuestion.Create(
                                English.definite_fragment adjective noun case,
                                Deutsch.definite_fragment adjective noun case
                            )
                            if not noun.Guts.IsPlural then
                                yield GermanPracticeQuestion.Create(
                                    English.indefinite_fragment adjective noun case,
                                    Deutsch.indefinite_fragment adjective noun case
                                )
                }
                |> Seq.randomShuffle
                |> Seq.truncate 50
                |> Array.ofSeq
        }
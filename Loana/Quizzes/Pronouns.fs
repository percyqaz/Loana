namespace Loana.Quizzes

open Loana.Language

module Pronouns =

    let PERSONAL : Quiz =
        {
            Name = "Personal Pronouns"
            Key = "quiz-personal-pronouns"
            Questions = fun () ->
                seq {
                    for case in [Case.Dative; Case.Accusative; Case.Nominative] do
                        for person in Person.LIST do
                            yield GermanPracticeQuestion.Create(English.personal_pronoun person case, Deutsch.personal_pronoun person case)
                }
                |> Array.ofSeq
        }

    let REFLEXIVE : Quiz =
        {
            Name = "Reflexive Pronouns"
            Key = "quiz-reflexive-pronouns"
            Questions = fun () ->
                seq {
                    for case in [false; true] do
                        for person in Person.LIST do
                            yield GermanPracticeQuestion.Create(English.reflexive_pronoun person case, Deutsch.reflexive_pronoun person case)
                }
                |> Array.ofSeq
        }

    let POSSESSIVE : Quiz =
        {
            Name = "Possessive Pronouns"
            Key = "quiz-possessive-pronouns"
            Questions = fun () ->
                seq {
                    let adjective = Some KLEIN
                    for case in [Case.Dative; Case.Accusative; Case.Nominative] do
                        for person in Person.LIST do
                            for noun in NOUNS do
                                yield GermanPracticeQuestion.Create(
                                    English.possessive_fragment person adjective noun case,
                                    Deutsch.possessive_fragment person adjective noun case
                                )
                }
                |> Seq.randomShuffle
                |> Seq.truncate 50
                |> Array.ofSeq
        }
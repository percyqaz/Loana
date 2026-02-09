namespace Loana.Quizzes

open System
open System.Drawing
open Loana.CLI
open Loana.Language

type ReflexivePronounsQuiz() =

    member this.Study() =

        let mutable loop = true
        while loop do
            Console.WriteLine(sprintf "Studying: Reflexive Pronouns", Color.LimeGreen)

            match Console.ReadLine() with
            | "back" ->
                loop <- false
            | "ok" ->
                loop <- false
                seq {
                    for person in Person.LIST do
                        for case in [false; true] do
                            yield GermanPracticeCard.Create(English.reflexive_pronoun person case, Deutsch.reflexive_pronoun person case)
                }
                |> Array.ofSeq
                |> fun cs -> QuizSession("Quiz", cs).Start()
            | _ -> ()
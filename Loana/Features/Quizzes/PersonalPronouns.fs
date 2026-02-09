namespace Loana.Features

open System
open System.Drawing
open Loana.CLI
open Loana.Language

type PersonalPronounsQuiz() =

    let mutable cases = Set.ofList [Case.Dative; Case.Accusative; Case.Nominative]

    member this.Study() =

        let mutable loop = true
        while loop do
            Console.WriteLine(sprintf "Studying: Personal Pronouns, %s" (String.concat ", " (cases |> Seq.map (sprintf "%A"))), Color.LimeGreen)
            match Console.ReadLine() with
            | "-nominative" -> cases <- cases.Remove Case.Nominative
            | "+nominative" -> cases <- cases.Add Case.Nominative
            | "-accusative" -> cases <- cases.Remove Case.Accusative
            | "+accusative" -> cases <- cases.Add Case.Nominative
            | "-dative" -> cases <- cases.Remove Case.Dative
            | "+dative" -> cases <- cases.Add Case.Dative
            | "back" ->
                loop <- false
            | "ok" ->
                loop <- false
                seq {
                    for person in Person.LIST do
                        for case in cases do
                            yield GermanPracticeCard.Create(English.personal_pronoun person case, Deutsch.personal_pronoun person case)
                }
                |> Array.ofSeq
                |> fun cs -> QuizSession("Quiz", cs).Start()
            | _ -> ()
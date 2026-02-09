namespace Loana.Quizzes

open System
open System.Drawing
open Loana.CLI
open Loana.Language

type PossessivePronounsQuiz() =

    let mutable cases = Set.ofList [Case.Dative; Case.Accusative; Case.Nominative; Case.Genitive]
    let mutable adjective = false

    member this.Study() : int option =

        let mutable result = None
        let mutable loop = true
        while loop do
            Console.WriteLine(sprintf "Studying: Possessive Pronouns", Color.LimeGreen)
            Console.WriteLine(String.concat ", " (cases |> Seq.map (sprintf "%A")), Color.LimeGreen)
            if adjective then
                Console.WriteLine(" + adjective", Color.LimeGreen)

            match Console.ReadLine() with
            | "-nominative" -> cases <- cases.Remove Case.Nominative
            | "+nominative" -> cases <- cases.Add Case.Nominative
            | "-accusative" -> cases <- cases.Remove Case.Accusative
            | "+accusative" -> cases <- cases.Add Case.Nominative
            | "-dative" -> cases <- cases.Remove Case.Dative
            | "+dative" -> cases <- cases.Add Case.Dative
            | "-genitive" -> cases <- cases.Remove Case.Genitive
            | "+genitive" -> cases <- cases.Add Case.Genitive
            | "-adjective" -> adjective <- false
            | "+adjective" -> adjective <- true
            | "back" ->
                loop <- false
            | "ok" ->
                loop <- false
                result <-
                    seq {
                        let adjective = if adjective then Some KLEIN else None
                        for person in Person.LIST do
                            for noun in NOUNS do
                                for case in cases do
                                    yield GermanPracticeCard.Create(
                                        English.possessive_fragment person adjective noun case,
                                        Deutsch.possessive_fragment person adjective noun case
                                    )
                    }
                    |> Seq.randomShuffle
                    |> Seq.truncate 50
                    |> Array.ofSeq
                    |> fun cs -> QuizSession("Possessive Pronouns", cs).Start()
            | _ -> ()

        result
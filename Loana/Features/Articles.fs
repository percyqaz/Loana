namespace Loana.Decks

open System
open System.Drawing
open Loana.CLI
open Loana.Language

type ArticlesQuiz() =

    let mutable cases = Set.ofList [Case.Dative; Case.Accusative; Case.Nominative; Case.Genitive]
    let mutable adjective = false
    let mutable definite = true
    let mutable indefinite = false

    member this.Study() =

        let mutable loop = true
        while loop do
            Console.WriteLine(sprintf "Studying: Possessive Pronouns", Color.LimeGreen)
            Console.WriteLine(String.concat ", " (cases |> Seq.map (sprintf "%A")), Color.LimeGreen)
            Console.WriteLine((if definite then "definite" else "") + " | " + (if indefinite then "indefinite" else ""), Color.LimeGreen)
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
            | "-definite" -> definite <- false
            | "+definite" -> definite <- true
            | "-indefinite" -> indefinite <- false
            | "+indefinite" -> indefinite <- true
            | "back" ->
                loop <- false
            | "ok" ->
                loop <- false
                seq {
                    let adjective = if adjective then Some KLEIN else None
                    for noun in NOUNS do
                        for case in cases do
                            if definite then
                                yield GermanPracticeCard.Create(
                                    English.definite_fragment adjective noun case,
                                    Deutsch.definite_fragment adjective noun case
                                )
                            if indefinite && not noun.Guts.IsPlural then
                                yield GermanPracticeCard.Create(
                                    English.indefinite_fragment adjective noun case,
                                    Deutsch.indefinite_fragment adjective noun case
                                )
                }
                |> Seq.randomShuffle
                |> Seq.truncate 50
                |> Array.ofSeq
                |> CliReviewSession
                |> _.Start()
            | _ -> ()
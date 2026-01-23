open System
open System.Drawing
open Loana.CLI
open Loana.Language
open Loana.Scheduler
open Loana.Features

let scheduler = ReviewSchedule("C:/Users/percy/Desktop/Source/Anki/Deutsch/cards.dat")
let wordlist = Wordlist()
wordlist.ReadDirectory("C:/Users/percy/Desktop/Source/Loana/Wordlists")

Console.WriteLine("Loana startup successful! Press enter to begin", Color.Yellow)
Console.ReadLine() |> ignore

SelectMenu(
    [|
        { Name = "Vocab"; Action = fun () -> VocabDeck(scheduler, wordlist).Study() }
        { Name = "Quizzes"; Action = fun () -> QuizScheduler(scheduler).Study() }
    |]
).Show()

//let machen = { Infinitive = Vocab.Parse "machen = to make, to do"; Separable = false; Tag = VerbTag.Transitive; Inflections = Map.empty }
//let essen = { Infinitive = Vocab.Parse "essen = to eat"; Separable = false; Tag = VerbTag.Transitive; Inflections = Map.empty }
//let abwaschen = { Infinitive = Vocab.Parse "abwaschen = to wash up"; Separable = true; Tag = VerbTag.Intransitive; Inflections = Map.empty }
//let sich_hinlegen = { Infinitive = Vocab.Parse "sich hinlegen = to lie down"; Separable = true; Tag = VerbTag.Intransitive; Inflections = Map.empty }

//printfn "%A" (VerbDownloader.extend_verb(machen, output))
//printfn "%A" (VerbDownloader.extend_verb(essen, output))
//printfn "%A" (VerbDownloader.extend_verb(abwaschen, output))
//printfn "%A" (VerbDownloader.extend_verb(sich_hinlegen, output))
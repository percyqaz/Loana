open System.IO
open Loana.CLI
open Loana.Language
open Loana.Study
open Loana.Features

type Loana =
    static member GetFilePath([<System.Runtime.CompilerServices.CallerFilePath>] ?path: string) =
        System.IO.Path.GetDirectoryName(path.Value)

let data_path = Path.Combine(Loana.GetFilePath(), "../Data")

let scheduler = ReviewSchedule(Path.Combine(data_path, "cards.dat"))
let wordlist = WordBank.ReadDirectory(Path.Combine(data_path, "Vocab"))
let sentence_list = WordBank.ReadDirectory(Path.Combine(data_path, "B1-Goethe"))
wordlist.Stats()
let vocab_deck = VocabDeck(scheduler, wordlist)
let b1_deck = VocabDeck(scheduler, sentence_list)

SelectMenu(
    [|
        { Name = "Vocab"; Action = fun () -> vocab_deck.Study() }
        { Name = "Quizzes"; Action = fun () -> QuizScheduler(scheduler).Study() }
        { Name = "Add Verbs"; Action = fun () -> Verbs.AddVerbs() }
    |]
).Show()
open Loana.CLI
open Loana.Language
open Loana.Study
open Loana.Features

let scheduler = ReviewSchedule("C:/Users/percy/Desktop/Source/Anki/Deutsch/cards.dat")
let wordlist = Wordlist()
wordlist.ReadDirectory("C:/Users/percy/Desktop/Source/Loana/Wordlists")
wordlist.Stats()
let vocab_deck = VocabDeck(scheduler, wordlist)

SelectMenu(
    [|
        { Name = "Vocab"; Action = fun () -> vocab_deck.Study() }
        { Name = "Quizzes"; Action = fun () -> QuizScheduler(scheduler).Study() }
        { Name = "Add Verbs"; Action = fun () -> Verbs.AddVerbs() }
    |]
).Show()

//let now = System.DateTimeOffset.UtcNow.ToUnixTimeSeconds()
//let mutable c = 0
//for card in vocab_deck.AvailableCards(["gcse-general"; "gcse-relationships-home"]) do
//    if card.Key.StartsWith("vocab-") then
//        scheduler.Schedule(card.Key, ReviewData.SeedAtLevel(now, 5))
//        c <- c + 1
//Console.WriteLine(sprintf "Seeded in %i existing cards at level 5" c, System.Drawing.Color.LightGreen)
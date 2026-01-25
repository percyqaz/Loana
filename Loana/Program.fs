open Loana.CLI
open Loana.Language
open Loana.Scheduler
open Loana.Features

let scheduler = ReviewSchedule("C:/Users/percy/Desktop/Source/Anki/Deutsch/cards.dat")
let wordlist = Wordlist()
wordlist.ReadDirectory("C:/Users/percy/Desktop/Source/Loana/Wordlists")
let vocab_deck = VocabDeck(scheduler, wordlist)

//let now = System.DateTimeOffset.UtcNow.ToUnixTimeSeconds()
//let mutable c = 0
//for card in vocab_deck.AvailableCards(["gcse-general"; "gcse-relationships-home"]) do
//    if card.Key.StartsWith("vocab-") then
//        scheduler.Schedule(card.Key, ReviewData.SeedAtLevel(now, 5))
//        c <- c + 1
//Console.WriteLine(sprintf "Seeded in %i existing cards at level 5" c, System.Drawing.Color.LightGreen)

wordlist.Stats()

SelectMenu(
    [|
        { Name = "Vocab"; Action = fun () -> vocab_deck.Study() }
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
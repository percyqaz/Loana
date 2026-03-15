namespace Loana.Verbs

open System
open System.Drawing
open Loana.CLI
open Loana.Language
open Loana.Data

type VerbDeckEntry =
    { Verb: Verb; Quiz: VerbQuiz }
    member this.Key = sprintf "verb-%O-%s" this.Quiz this.Verb.Infinitive.Key

type VerbDeck(scheduler: ReviewSchedule, words: WordBank, verbs: VerbBank) =
    
    member this.Scheduler = scheduler

    member inline this.LevelOf(c: VerbDeckEntry) : int =
        match this.Scheduler.Get c.Key with
        | ValueSome data -> data.Level
        | ValueNone -> 0

    member this.AvailableCards() : VerbDeckEntry seq =
        seq {
            for word in words.Entries do
                match word.Item with
                | Verb v ->
                    for q in v.Quizzes do
                        yield { Verb = v; Quiz = q }
                | _ -> ()
        }
        |> Seq.cache

    member inline this.LearningCards(cards: VerbDeckEntry seq) =
        cards
        |> Seq.where (fun c -> (this.Scheduler.Get c.Key).IsNone)

    member inline this.ReviewCards(cards: VerbDeckEntry seq) =
        cards
        |> Seq.where (fun c -> (this.Scheduler.Get c.Key).IsSome)

    member inline this.DueReviewCards(cards: VerbDeckEntry seq, now: int64) =
        cards
        |> Seq.choose (fun c ->
            match this.Scheduler.Get c.Key with
            | ValueSome data ->
                let dl = data.DueLevel now
                if dl >= 0 then Some (c, dl) else None
            | ValueNone -> None
        )
        |> Seq.sortByDescending snd
        |> Seq.map fst

    member inline this.AheadReviewCards(cards: VerbDeckEntry seq, now: int64) =
        cards
        |> Seq.choose (fun c ->
            match this.Scheduler.Get c.Key with
            | ValueSome data ->
                let n = data.NextReview
                if n > now then Some (c, n) else None
            | ValueNone -> None
        )
        |> Seq.sortBy snd
        |> Seq.map fst

    member this.Review (cards: VerbDeckEntry seq) =
        let cards =
            this.DueReviewCards(cards, DateTimeOffset.UtcNow.ToUnixTimeSeconds())
            |> Seq.truncate 5
            |> ResizeArray
            
        while cards.Count > 0 do
            let verb = cards.[0]
            cards.RemoveAt(0)
            
            let verb_cards =
                verbs.Ensure(verb.Verb)
                |> Map.toSeq
                |> Seq.filter (fun (i, _) -> i.AsQuiz = verb.Quiz)
                |> Seq.map (fun (i, text) -> VerbCard.C_Inflection(verb.Verb, i, text))
                |> Array.ofSeq
                
            let session = VerbSession(verb_cards)
            let result = session.Start()
            if result.EndEarly then
                cards.Clear()
            else
                if result.NotGood = 0 then
                    scheduler.Reschedule(verb.Key, _.Promote) |> session.Log
                elif result.NotGood = 1 then
                    scheduler.Reschedule(verb.Key, _.Keep) |> session.Log
                elif result.Forgot > 0 then
                    scheduler.Reschedule(verb.Key, _.Forget) |> session.Log
                else
                    scheduler.Reschedule(verb.Key, _.Demote) |> session.Log
                    
        Console.WriteLine(MenuRender.Pad "Session ended.", Color.LightGreen, Color.FromArgb(0x303030))
        Console.ReadKey(true) |> ignore
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

    member this.AvailableEntries() : VerbDeckEntry seq =
        seq {
            for word in words.Entries do
                match word.Item with
                | Verb v ->
                    for q in v.Quizzes do
                        yield { Verb = v; Quiz = q }
                | _ -> ()
        }
        |> Seq.cache

    member inline this.LearningEntries(entries: VerbDeckEntry seq) =
        entries
        |> Seq.where (fun c -> (this.Scheduler.Get c.Key).IsNone)

    member inline this.ReviewEntries(entries: VerbDeckEntry seq) =
        entries
        |> Seq.where (fun c -> (this.Scheduler.Get c.Key).IsSome)

    member inline this.DueReviewEntries(entries: VerbDeckEntry seq, now: int64) =
        entries
        |> Seq.choose (fun c ->
            match this.Scheduler.Get c.Key with
            | ValueSome data ->
                let dl = data.DueLevel now
                if dl >= 0 then Some (c, dl) else None
            | ValueNone -> None
        )
        |> Seq.sortByDescending snd
        |> Seq.map fst

    member inline this.AheadReviewEntries(entries: VerbDeckEntry seq, now: int64) =
        entries
        |> Seq.choose (fun c ->
            match this.Scheduler.Get c.Key with
            | ValueSome data ->
                let n = data.NextReview
                if n > now then Some (c, n) else None
            | ValueNone -> None
        )
        |> Seq.sortBy snd
        |> Seq.map fst
        
    member this.Learn (entries: VerbDeckEntry seq) =
        let to_learn =
            this.LearningEntries(entries)
            |> Seq.tryHead
            
        match to_learn with
        | None -> ()
        | Some verb ->
            let verb_cards =
                verbs.Ensure(verb.Verb)
                |> Map.toSeq
                |> Seq.filter (fun (i, _) -> i.AsQuiz = verb.Quiz)
                |> Seq.map (fun (i, text) -> VerbCard.C_Inflection(verb.Verb, i, text))
                |> Array.ofSeq
                
            let session = VerbSession(verb_cards)
            let result = session.Start()
            if not result.EndEarly then
                let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
                scheduler.Schedule(verb.Key, ReviewData.Level1(now, (1 + result.NotGood) |> min 10 |> max 1), now) |> session.Log
                    
        Console.WriteLine(MenuRender.Pad "Session ended.", Color.LightGreen, Color.FromArgb(0xFF_303030))
        Console.ReadKey(true) |> ignore

    member this.Review (entries: VerbDeckEntry seq) =
        let session_entries =
            this.DueReviewEntries(entries, DateTimeOffset.UtcNow.ToUnixTimeSeconds())
            |> Seq.truncate 5
            |> ResizeArray
            
        while session_entries.Count > 0 do
            let verb = session_entries.[0]
            session_entries.RemoveAt(0)
            
            let verb_cards =
                verbs.Ensure(verb.Verb)
                |> Map.toSeq
                |> Seq.filter (fun (i, _) -> i.AsQuiz = verb.Quiz)
                |> Seq.map (fun (i, text) -> VerbCard.C_Inflection(verb.Verb, i, text))
                |> Array.ofSeq
                
            let session = VerbSession(verb_cards)
            let result = session.Start()
            if result.EndEarly then
                session_entries.Clear()
            else
                if result.NotGood = 0 then
                    scheduler.Reschedule(verb.Key, _.Promote) |> session.Log
                elif result.NotGood = 1 then
                    scheduler.Reschedule(verb.Key, _.Keep) |> session.Log
                elif result.Forgot > 0 then
                    scheduler.Reschedule(verb.Key, _.Forget) |> session.Log
                else
                    scheduler.Reschedule(verb.Key, _.Demote) |> session.Log
                    
        Console.WriteLine(MenuRender.Pad "Session ended.", Color.LightGreen, Color.FromArgb(0xFF_303030))
        Console.ReadKey(true) |> ignore
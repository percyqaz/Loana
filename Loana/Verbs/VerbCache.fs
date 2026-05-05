namespace Loana.Verbs

open Loana.Language
open Loana.Data

type VerbCacheEntry =
    { Verb: Verb; Quiz: VerbQuiz }
    member this.Key = sprintf "verb-%O-%s" this.Quiz this.Verb.Infinitive.Key

type VerbCache(scheduler: ReviewSchedule, words: WordBank) =
    
    member this.Scheduler = scheduler

    member inline this.LevelOf(c: VerbCacheEntry) : int =
        match this.Scheduler.Get c.Key with
        | ValueSome data -> data.Level
        | ValueNone -> 0

    member this.AvailableEntries() : VerbCacheEntry seq =
        seq {
            for word in words.Entries do
                match word.Item with
                | Verb v ->
                    for q in v.Quizzes do
                        yield { Verb = v; Quiz = q }
                | _ -> ()
        }
        |> Seq.cache

    member inline this.LearningEntries(entries: VerbCacheEntry seq) =
        entries
        |> Seq.where (fun c -> (this.Scheduler.Get c.Key).IsNone)

    member inline this.ReviewEntries(entries: VerbCacheEntry seq) =
        entries
        |> Seq.where (fun c -> (this.Scheduler.Get c.Key).IsSome)

    member inline this.DueReviewEntries(entries: VerbCacheEntry seq, now: int64) =
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

    member inline this.AheadReviewEntries(entries: VerbCacheEntry seq, now: int64) =
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
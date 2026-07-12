namespace Loana.Verbs

open Loana.Language
open Loana.Data

type VerbCacheEntry =
    {
        Verb: Verb
        Tense: VerbTense
    }

    member this.Key =
        sprintf "verb-%O-%s" this.Tense this.Verb.Infinitive.DeutschAsciiIdentifier

type VerbCache(scheduler: ReviewSchedule, words: WordBank) =

    member this.Scheduler = scheduler

    member inline this.LevelOf(cached_verb: VerbCacheEntry) : int =
        match this.Scheduler.Get(cached_verb.Key) with
        | ValueSome data -> data.Level
        | ValueNone -> 0

    member this.AvailableEntries() : VerbCacheEntry seq =
        seq {
            for word in words.Entries do
                match word.Item with
                | Verb verb ->
                    for verb_tense in verb.Tenses do
                        yield { Verb = verb; Tense = verb_tense }
                | _ -> ()
        }
        |> Seq.cache

    member inline this.LearningEntries(entries: VerbCacheEntry seq) =
        entries |> Seq.where(fun cached_verb -> (this.Scheduler.Get(cached_verb.Key)).IsNone)

    member inline this.ReviewEntries(entries: VerbCacheEntry seq) =
        entries |> Seq.where(fun cached_verb -> (this.Scheduler.Get(cached_verb.Key)).IsSome)

    member inline this.DueReviewEntries(entries: VerbCacheEntry seq, now: int64) =

        let priority_or_none (cached_verb: VerbCacheEntry) : int voption =
            this.Scheduler.Get(cached_verb.Key) |> ValueOption.map(_.OverduePriority(now)) |> ValueOption.filter((>=) 0)

        entries
        |> Seq.choose(fun cached_verb ->
            match priority_or_none(cached_verb) with
            | ValueSome priority -> Some(cached_verb, priority)
            | ValueNone -> None
        )
        |> Seq.sortByDescending snd
        |> Seq.map fst

    member inline this.AheadReviewEntries(entries: VerbCacheEntry seq, now: int64) =

        let next_review_or_none (cached_verb: VerbCacheEntry) : int64 voption =
            this.Scheduler.Get(cached_verb.Key) |> ValueOption.map(_.NextReview) |> ValueOption.filter((>) now)

        let verbs_asc_by_next_review =
            entries
            |> Seq.choose(fun cached_verb ->
                match next_review_or_none(cached_verb) with
                | ValueSome next_review -> Some(cached_verb, next_review)
                | ValueNone -> None
            )
            |> Seq.sortBy snd
            |> Seq.map fst

        verbs_asc_by_next_review

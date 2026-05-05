namespace Loana.Vocab

open Loana.CLI
open Loana.Language
open Loana.Data

type Chore =
    { Message: string; Urgent: bool }
    static member urgent message = { Message = message; Urgent = true }
    static member non_urgent message = { Message = message; Urgent = false }

type VocabDeck(scheduler: ReviewSchedule, words: WordBank) =

    static let mutable study_size_multiplier = 5

    member this.Scheduler = scheduler

    member inline this.LevelOf<^T when ^T : (member Key: string)>(c: ^T) : int =
        match this.Scheduler.Get c.Key with
        | ValueSome data -> data.Level
        | ValueNone -> 0

    member private this.AvailableCards(v: Vocab) : Card seq =
        seq {
            let tier_1 = VocabCard.M_Tier1_RecogniseDE(v)
            yield tier_1

            if this.LevelOf tier_1 >= 2 then
                yield VocabCard.M_Tier2_RecallDE(v)
        }

    member private this.AvailableCards(v: Verb) : Card seq =
        seq {
            let tier_1 = VocabCard.M_Tier1_RecogniseDE(v.Infinitive)
            let tier_2 = VocabCard.M_Tier2_RecallDE(v.Infinitive)
            yield tier_1

            if this.LevelOf tier_1 >= 2 then
                yield tier_2

            match v.PastParticiple with
            | Something pp ->
                let tier_3 = VocabCard.M_Tier3_RecognisePastParticipleDE(pp)
                let tier_4 = VocabCard.M_Tier4_RecallPastParticipleDE(pp)
                if this.LevelOf tier_2 >= 4 then
                    yield tier_3
                if this.LevelOf tier_3 >= 2 then
                    yield tier_4
            | _ -> ()
        }

    member private this.AvailableCards(n: Noun) : Card seq =
        seq {
            let tier_1 = VocabCard.M_Tier1_RecogniseDE(n.Translation)
            let tier_2 = VocabCard.M_Tier2_RecallDE(n.Translation)
            let tier_3 = VocabCard.M_Tier3_RecogniseArticleDE(n)
            let tier_4 = VocabCard.M_Tier4_RecallArticleDE(n)

            if this.LevelOf tier_1 < 2 then
                yield tier_1
            elif this.LevelOf tier_2 < 4 then
                yield tier_1
                yield tier_2
            elif this.LevelOf tier_3 < 2 then
                yield tier_2
                yield tier_3
            else
                yield tier_3
                yield tier_4

            match n.PluralForm with
            | Some p ->
                let tier_5 = VocabCard.M_Tier5_RecognisePluralDE(p)
                let tier_6 = VocabCard.M_Tier6_RecallPluralDE(p)
                if this.LevelOf tier_4 >= 2 then
                    yield tier_5
                if this.LevelOf tier_5 >= 2 then
                    yield tier_6
            | None -> ()
        }

    member private this.AvailableCards(word: WordlistItem): Card seq =
        match word with
        | Vocab v -> this.AvailableCards v
        | Noun n -> this.AvailableCards n
        | Verb v -> this.AvailableCards v

    member this.AvailableCards(sources: string list) : Card seq =
        seq {
            for word in words.Entries do
                if sources.IsEmpty || List.contains word.Source.File sources then
                    yield! this.AvailableCards(word.Item)
        }
        |> Seq.cache

    member this.AvailableCards() = this.AvailableCards([])

    member private this.PossibleCards(v: Vocab) : Card seq =
        seq {
            yield VocabCard.M_Tier1_RecogniseDE(v)
            yield VocabCard.M_Tier2_RecallDE(v)
        }

    member private this.PossibleCards(v: Verb) : Card seq =
        seq {
            yield VocabCard.M_Tier1_RecogniseDE(v.Infinitive)
            yield VocabCard.M_Tier2_RecallDE(v.Infinitive)
            match v.PastParticiple with
            | Something pp ->
                yield VocabCard.M_Tier3_RecognisePastParticipleDE(pp)
                yield VocabCard.M_Tier4_RecallPastParticipleDE(pp)
            | _ -> ()
        }

    member private this.PossibleCards(n: Noun) : Card seq =
        seq {
            yield VocabCard.M_Tier1_RecogniseDE(n.Translation)
            yield VocabCard.M_Tier2_RecallDE(n.Translation)
            yield VocabCard.M_Tier3_RecogniseArticleDE(n)
            yield VocabCard.M_Tier4_RecallArticleDE(n)
            match n.PluralForm with
            | Some p ->
                yield VocabCard.M_Tier5_RecognisePluralDE(p)
                yield VocabCard.M_Tier6_RecallPluralDE(p)
            | None -> ()
        }

    member private this.PossibleCards(word: WordlistItem) : Card seq =
        match word with
        | Vocab v -> this.PossibleCards v
        | Noun n -> this.PossibleCards n
        | Verb v -> this.PossibleCards v

    member this.PossibleCards(sources: string list) : Card seq =
        seq {
            for word in words.Entries do
                if sources.IsEmpty || List.contains word.Source.File sources then
                    yield! this.PossibleCards(word.Item)
        }
        |> Seq.cache

    member this.PossibleCards() = this.PossibleCards([])

    member this.FilterByTier(cards: Card seq, min_tier: int, max_tier: int) =
        cards
        |> Seq.where (fun c -> c.Tier >= min_tier && c.Tier <= max_tier)

    member this.FilterByLevel(cards: Card seq, minlevel: int, maxlevel: int) =
        cards
        |> Seq.where (fun c ->
            match scheduler.Get c.Key with
            | ValueSome data -> data.Level >= minlevel && data.Level <= maxlevel
            | ValueNone -> false
        )

    member this.Chores() : Chore seq =
        seq {
            for word in words.Entries do
                match word.Item with
                | Vocab v when v.DetectNoun ->
                    let message = sprintf "'%O' in '%s' is missing gender!" v.Deutsch word.Source.File
                    if this.LevelOf(VocabCard.M_Tier2_RecallDE(v)) >= 4 then yield Chore.urgent message
                    else yield Chore.non_urgent message
                | Noun n when n.Plural.IsToBeDetermined ->
                    let message = sprintf "'%O' in '%s' is missing plural (or no_plural marker)!" n.Deutsch word.Source.File
                    yield Chore.non_urgent message
                | _ -> ()
        }

    member inline this.LearningCards<^T when ^T : (member Key: string)>(cards: ^T seq) =
        cards
        |> Seq.where (fun c -> (this.Scheduler.Get c.Key).IsNone && not(this.Scheduler.IsBuried c.Key))

    member inline this.ReviewCards<^T when ^T : (member Key: string)>(cards: ^T seq) =
        cards
        |> Seq.where (fun c -> (this.Scheduler.Get c.Key).IsSome)

    member inline this.DueReviewCards<^T when ^T : (member Key: string)>(cards: ^T seq, now: int64) =
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

    member inline this.AheadReviewCards<^T when ^T : (member Key: string)>(cards: ^T seq, now: int64) =
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

    member inline this.LevelDistribution<^T when ^T : (member Key: string)>(cards: ^T seq) : (int * int) seq =
        cards
        |> Seq.map this.LevelOf
        |> Seq.countBy id
        |> Seq.sortBy fst

    member this.LearnBatchSize = 4 * study_size_multiplier
    member this.ReviewBatchSize = 10 * study_size_multiplier

    member this.IncreaseBatchSize() =
        study_size_multiplier <- study_size_multiplier + 1 |> min 20

    member this.DecreaseBatchSize() =
        study_size_multiplier <- study_size_multiplier - 1 |> max 1
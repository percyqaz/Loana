namespace Loana.Vocab

open Loana.Language
open Loana.Data

type Chore =
    { Message: string; IsUrgent: bool }
    static member Urgent(message: string) : Chore = { Message = message; IsUrgent = true }
    static member NonUrgent(message: string) : Chore = { Message = message; IsUrgent = false }

type VocabDeck(scheduler: ReviewSchedule, words: WordBank) =

    // todo: should not be here
    static let mutable study_size_multiplier = 3

    member this.Scheduler = scheduler

    member inline this.LevelOf(card: Card) : int =
        match this.Scheduler.Get card.Key with
        | ValueSome data -> data.Level
        | ValueNone -> 0

    member private this.AvailableCards(vocab: Vocab) : Card seq =
        seq {
            let tier_1 = VocabCard.M_Tier1_RecogniseDE(vocab)
            yield tier_1

            if this.LevelOf tier_1 >= 2 then
                yield VocabCard.M_Tier2_RecallDE(vocab)
        }

    member private this.AvailableCards(verb: Verb) : Card seq =
        seq {
            let tier_1 = VocabCard.M_Tier1_RecogniseDE(verb.Infinitive)
            let tier_2 = VocabCard.M_Tier2_RecallDE(verb.Infinitive)
            yield tier_1

            if this.LevelOf tier_1 >= 2 then
                yield tier_2

            match verb.PastParticiple with
            | KnownValue past_particle ->
                let tier_3 = VocabCard.M_Tier3_RecognisePastParticipleDE(past_particle)
                let tier_4 = VocabCard.M_Tier4_RecallPastParticipleDE(past_particle)
                if this.LevelOf tier_2 >= 4 then
                    yield tier_3
                if this.LevelOf tier_3 >= 2 then
                    yield tier_4
            | _ -> ()
        }

    member private this.AvailableCards(noun: Noun) : Card seq =
        seq {
            let tier_1 = VocabCard.M_Tier1_RecogniseDE(noun.Translation)
            let tier_2 = VocabCard.M_Tier2_RecallDE(noun.Translation)
            let tier_3 = VocabCard.M_Tier3_RecogniseArticleDE(noun)
            let tier_4 = VocabCard.M_Tier4_RecallArticleDE(noun)

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

            match noun.PluralForm with
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
        | Vocab vocab -> this.AvailableCards vocab
        | Noun noun -> this.AvailableCards noun
        | Verb verb -> this.AvailableCards verb

    member this.AvailableCards(sources: string seq) : Card seq =
        let sources_set = Set.ofSeq sources
        seq {
            for word in words.Entries do
                if sources_set.IsEmpty || sources_set.Contains word.Source.File then
                    yield! this.AvailableCards(word.Item)
        }
        |> Seq.cache

    member this.AvailableCards() = this.AvailableCards([])

    member private this.PossibleCards(vocab: Vocab) : Card seq =
        seq {
            yield VocabCard.M_Tier1_RecogniseDE(vocab)
            yield VocabCard.M_Tier2_RecallDE(vocab)
        }

    member private this.PossibleCards(verb: Verb) : Card seq =
        seq {
            yield VocabCard.M_Tier1_RecogniseDE(verb.Infinitive)
            yield VocabCard.M_Tier2_RecallDE(verb.Infinitive)
            match verb.PastParticiple with
            | KnownValue past_participle ->
                yield VocabCard.M_Tier3_RecognisePastParticipleDE(past_participle)
                yield VocabCard.M_Tier4_RecallPastParticipleDE(past_participle)
            | _ -> ()
        }

    member private this.PossibleCards(noun: Noun) : Card seq =
        seq {
            yield VocabCard.M_Tier1_RecogniseDE(noun.Translation)
            yield VocabCard.M_Tier2_RecallDE(noun.Translation)
            yield VocabCard.M_Tier3_RecogniseArticleDE(noun)
            yield VocabCard.M_Tier4_RecallArticleDE(noun)
            match noun.PluralForm with
            | Some plural ->
                yield VocabCard.M_Tier5_RecognisePluralDE(plural)
                yield VocabCard.M_Tier6_RecallPluralDE(plural)
            | None -> ()
        }

    member private this.PossibleCards(word: WordlistItem) : Card seq =
        match word with
        | Vocab vocab -> this.PossibleCards vocab
        | Noun noun -> this.PossibleCards noun
        | Verb verb -> this.PossibleCards verb

    member this.PossibleCards(sources: string seq) : Card seq =
        let sources_set = Set.ofSeq sources
        seq {
            for word in words.Entries do
                if sources_set.IsEmpty || sources_set.Contains word.Source.File then
                    yield! this.PossibleCards(word.Item)
        }
        |> Seq.cache

    member this.PossibleCards() = this.PossibleCards([])

    member this.FilterByTier(cards: Card seq, min_tier: int, max_tier: int) =
        cards
        |> Seq.where (fun card -> card.Tier >= min_tier && card.Tier <= max_tier)

    member this.FilterByLevel(cards: Card seq, minlevel: int, maxlevel: int) =
        cards
        |> Seq.where (fun card ->
            match scheduler.Get card.Key with
            | ValueSome data -> data.Level >= minlevel && data.Level <= maxlevel
            | ValueNone -> false
        )

    member this.Chores() : Chore seq =
        seq {
            for word in words.Entries do
                match word.Item with
                | Vocab vocab when vocab.LooksLikeANoun ->
                    let message = sprintf "'%O' in '%s' is missing gender!" vocab.Deutsch word.Source.File
                    if this.LevelOf(VocabCard.M_Tier2_RecallDE(vocab)) >= 4 then yield Chore.Urgent message
                    else yield Chore.NonUrgent message
                | Noun noun when noun.Plural.IsUnknown ->
                    let message = sprintf "'%O' in '%s' is missing plural (or no_plural marker)!" noun.Deutsch word.Source.File
                    yield Chore.NonUrgent message
                | _ -> ()
        }

    member inline this.LearningCards(cards: Card seq) =
        cards
        |> Seq.where (fun card -> (this.Scheduler.Get card.Key).IsNone && not(this.Scheduler.IsBuried card.Key))

    member inline this.ReviewCards(cards: Card seq) =
        cards
        |> Seq.where (fun card -> (this.Scheduler.Get card.Key).IsSome)
        
    member inline this.DueReviewCards(cards: Card seq, now: int64) : Card array =

        let card_priority_or_none(card: Card) : int voption =
            this.Scheduler.Get card.Key
            |> ValueOption.map _.OverduePriority(now)
            |> ValueOption.filter (fun priority -> priority >= 0)

        let cards_desc_by_priority =
            cards
            |> Seq.choose (fun card ->
                match card_priority_or_none(card) with
                | ValueSome priority -> Some (card, priority)
                | ValueNone -> None
            )
            |> Seq.sortByDescending snd
            |> Seq.map fst
            |> Seq.toArray

        let filter_bumped_cards(cards: Card array) =
            let hidden = Set.ofSeq (Array.choose _.BumpKey cards)
            cards |> Array.filter (fun card -> not (hidden.Contains card.Key))

        filter_bumped_cards(cards_desc_by_priority)

    member inline this.AheadReviewCards(cards: Card seq, now: int64) : Card array =

        let card_next_review_or_none(card: Card) : int64 voption =
            this.Scheduler.Get card.Key
            |> ValueOption.map _.NextReview
            |> ValueOption.filter (fun next_review -> next_review > now)

        let cards_asc_by_next_review =
            cards
            |> Seq.choose (fun card ->
                match card_next_review_or_none(card) with
                | ValueSome next_review -> Some (card, next_review)
                | ValueNone -> None
            )
            |> Seq.sortBy snd
            |> Seq.map fst
            |> Seq.toArray

        cards_asc_by_next_review

    member inline this.LevelDistribution(cards: Card seq) : (int * int) seq =
        cards
        |> Seq.map this.LevelOf
        |> Seq.countBy id
        |> Seq.sortBy fst

    // todo: move UI state and functions to UI only
    member this.LearnBatchSize = 4 * study_size_multiplier
    member this.ReviewBatchSize = 10 * study_size_multiplier

    member this.IncreaseBatchSize() =
        study_size_multiplier <- study_size_multiplier + 1 |> min 20

    member this.DecreaseBatchSize() =
        study_size_multiplier <- study_size_multiplier - 1 |> max 1
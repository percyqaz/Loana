namespace Loana.Vocab

open System
open System.Drawing
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
            let tier_1 = VocabCard.C_Tier1_RecogniseDE(v)
            yield tier_1

            if this.LevelOf tier_1 >= 2 then
                yield VocabCard.C_Tier2_RecallDE(v)
        }

    member private this.AvailableCards(v: Verb) : Card seq =
        seq {
            let tier_1 = VocabCard.C_Tier1_RecogniseDE(v.Infinitive)
            let tier_2 = VocabCard.C_Tier2_RecallDE(v.Infinitive)
            yield tier_1

            if this.LevelOf tier_1 >= 2 then
                yield tier_2
                
            match v.PastParticiple with
            | Something pp ->
                let tier_3 = VocabCard.C_Tier3_RecognisePastParticipleDE(pp)
                let tier_4 = VocabCard.C_Tier4_RecallPastParticipleDE(pp)
                if this.LevelOf tier_2 >= 4 then
                    yield tier_3
                if this.LevelOf tier_3 >= 2 then
                    yield tier_4
            | _ -> ()
        }

    member private this.AvailableCards(n: Noun) : Card seq =
        seq {
            let tier_1 = VocabCard.C_Tier1_RecogniseDE(n.Translation)
            let tier_2 = VocabCard.C_Tier2_RecallDE(n.Translation)
            let tier_3 = VocabCard.C_Tier3_RecogniseArticleDE(n)
            let tier_4 = VocabCard.C_Tier4_RecallArticleDE(n)

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
                let tier_5 = VocabCard.C_Tier5_RecognisePluralDE(p)
                let tier_6 = VocabCard.C_Tier6_RecallPluralDE(p)
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

    member private this.PossibleCards(v: Vocab) : CardMeta seq =
        seq {
            yield VocabCard.M_Tier1_RecogniseDE(v)
            yield VocabCard.M_Tier2_RecallDE(v)
        }

    member private this.PossibleCards(v: Verb) : CardMeta seq =
        seq {
            yield VocabCard.M_Tier1_RecogniseDE(v.Infinitive)
            yield VocabCard.M_Tier2_RecallDE(v.Infinitive)
            match v.PastParticiple with
            | Something pp ->
                yield VocabCard.M_Tier3_RecognisePastParticipleDE(pp)
                yield VocabCard.M_Tier4_RecallPastParticipleDE(pp)
            | _ -> ()
        }

    member private this.PossibleCards(n: Noun) : CardMeta seq =
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

    member private this.PossibleCards(word: WordlistItem) : CardMeta seq =
        match word with
        | Vocab v -> this.PossibleCards v
        | Noun n -> this.PossibleCards n
        | Verb v -> this.PossibleCards v

    member this.PossibleCards(sources: string list) : CardMeta seq =
        seq {
            for word in words.Entries do
                if sources.IsEmpty || List.contains word.Source.File sources then
                    yield! this.PossibleCards(word.Item)
        }
        |> Seq.cache

    member this.PossibleCards() = this.PossibleCards([])

    member this.FilterByTier(cards: Card seq, min_tier: int, max_tier: int) =
        cards
        |> Seq.where (fun c -> c.Meta.Tier >= min_tier && c.Meta.Tier <= max_tier)

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

    member this.Review (cards: Card seq) =
        let cards =
            this.DueReviewCards(cards, DateTimeOffset.UtcNow.ToUnixTimeSeconds())
            |> Seq.distinctBy _.Meta.ReferenceKey
            |> Seq.truncate this.ReviewBatchSize
            |> Array.ofSeq
        if cards.Length > 0 then
            let result = ReviewSession(cards, scheduler, false).Start()
            Console.WriteLine(
                MenuRender.Pad (
                    sprintf "Session ended%s! [%i|%i|%i|%i] (%.1f%%)"
                        (if result.EndEarly then " early" else "")
                        result.Good result.Ok result.Bad result.NotGood
                        (100.0f * (float32 result.Good / ((float32 result.Good + float32 result.NotGood) |> max 1.0f)))
                ),
                Color.LightGreen,
                Color.FromArgb(0x303030)
            )
            Console.ReadKey(true) |> ignore

    member this.ReviewAhead (cards: Card seq) =
        let cards =
            this.AheadReviewCards(cards, DateTimeOffset.UtcNow.ToUnixTimeSeconds())
            |> Seq.distinctBy _.Meta.ReferenceKey
            |> Seq.truncate this.ReviewBatchSize
            |> Array.ofSeq
        if cards.Length > 0 then
            let result = ReviewSession(cards, scheduler, true).Start()
            Console.WriteLine(
                MenuRender.Pad (
                    sprintf "Session ended%s! [%i|%i|%i|%i] (%.1f%%)"
                        (if result.EndEarly then " early" else "")
                        result.Good result.Ok result.Bad result.NotGood
                        (100.0f * (float32 result.Good / ((float32 result.Good + float32 result.NotGood) |> max 1.0f)))
                ),
                Color.LightGreen,
                Color.FromArgb(0x303030)
            )
            Console.ReadKey(true) |> ignore

    member this.Learn (cards: Card seq) =
        let cards = this.LearningCards(cards) |> Seq.truncate this.LearnBatchSize |> Array.ofSeq
        if cards.Length > 0 then
            let result = LearnSession(cards, scheduler).Start()
            Console.WriteLine(
                MenuRender.Pad (
                    sprintf "Session ended%s! [%i|%i] (%.1f)"
                        (if result.EndEarly then " early" else "")
                        result.Good result.NotGood
                        (1.0f + float32 result.NotGood / (float32 result.Good |> max 1.0f))
                    ),
                Color.LightGreen,
                Color.FromArgb(0x303030)
            )
            Console.ReadKey(true) |> ignore

    member this.ChoresList () =
        Console.WriteLine(MenuRender.Pad " Chores list ", Color.White, Color.FromArgb(0x303030))
        let chores = this.Chores() |> Seq.cache
        let urgent = chores |> Seq.filter _.Urgent |> Seq.truncate 20 |> Array.ofSeq
        let non_urgent = chores |> Seq.filter (_.Urgent >> not) |> Seq.truncate 20 |> Array.ofSeq

        Console.WriteLine(MenuRender.Pad (sprintf " - %i Urgent - " urgent.Length), Color.LightGray, Color.FromArgb(0x202020))
        for chore in urgent do Console.WriteLine(chore.Message, Color.DeepPink)
        Console.WriteLine(MenuRender.Pad (sprintf " - %i Non-urgent - " non_urgent.Length), Color.LightGray, Color.FromArgb(0x202020))
        for chore in non_urgent do Console.WriteLine(chore.Message, Color.Yellow)

        Console.ReadKey(true) |> ignore

    member this.LearnBatchSize = 4 * study_size_multiplier
    member this.ReviewBatchSize = 10 * study_size_multiplier

    member this.IncreaseBatchSize() =
        study_size_multiplier <- study_size_multiplier + 1 |> min 20

    member this.DecreaseBatchSize() =
        study_size_multiplier <- study_size_multiplier - 1 |> max 1

    member this.Stats(all_cards: Card seq) : unit =
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()

        let by_hour =
            this.AheadReviewCards(all_cards, now)
            |> Seq.map (fun c -> (scheduler.Get c.Key).Value.NextReview - now)
            |> Seq.takeWhile (fun c -> c < TimeSpan.SecondsPerHour * int64 MenuRender.Width)
            |> Seq.countBy (fun c -> c / TimeSpan.SecondsPerHour)
            |> Map.ofSeq

        let by_day =
            this.AheadReviewCards(all_cards, now)
            |> Seq.map (fun c -> (scheduler.Get c.Key).Value.NextReview - now)
            |> Seq.takeWhile (fun c -> c < TimeSpan.SecondsPerDay * int64 MenuRender.Width)
            |> Seq.countBy (fun c -> c / TimeSpan.SecondsPerDay)
            |> Map.ofSeq

        let forgotten =
            all_cards
            |> Seq.choose (fun c -> match scheduler.Get c.Key with ValueSome v when (v.Reviews > v.Level && v.Level < 4) || v.Difficulty > 5 -> Some (v, c.Key) | _ -> None)
            |> Seq.sortByDescending (fst >> _.LastReviewed)
            |> Seq.truncate 20
            |> Array.ofSeq

        let upcoming_bar (data, threshold) =
            for i = 0 to MenuRender.Width - 1 do
                let hit = (Map.tryFind (int64 i) data |> Option.defaultValue 0) > threshold
                MenuRender.Write(" ", Color.White, if hit then Color.Green else Color.FromArgb(0x101010))
            MenuRender.WriteLine()

        MenuRender.WriteLine(MenuRender.Pad " Stats for selected deck(s) ", Color.LightGray, Color.FromArgb(0x303030))
        MenuRender.WriteLine(MenuRender.Pad " - Distribution - ", Color.LightGray, Color.FromArgb(0x202020))
        all_cards
        |> this.LevelDistribution
        |> Seq.iter (fun (level, count) ->
            MenuRender.Write(sprintf "[%i]" level, ReviewData.LevelColors.[level], Color.FromArgb(ReviewData.LevelColors.[level].ToArgb() / 2))
            MenuRender.Write(String.replicate (count / 100) " ", Color.White, ReviewData.LevelColors.[level])
            MenuRender.WriteLine((sprintf " %i cards" count).PadRight(MenuRender.Width - (count / 100) - 3), Color.LightGray, Color.FromArgb(0x101010))
        )

        MenuRender.WriteLine(MenuRender.Pad " - Upcoming workload (axis in days) - ", Color.LightGray, Color.FromArgb(0x303030))
        upcoming_bar(by_hour, 250)
        upcoming_bar(by_hour, 200)
        upcoming_bar(by_hour, 150)
        upcoming_bar(by_hour, 100)
        upcoming_bar(by_hour, 50)

        for i = 0 to MenuRender.Width / 24 - 1 do
            MenuRender.Write("�".PadLeft(12), Color.LightGray, Color.FromArgb(0x202020))
            MenuRender.Write((i + 1).ToString().PadLeft(12), Color.LightGray, Color.FromArgb(0x202020))
        MenuRender.Write("".PadLeft(MenuRender.Width - (MenuRender.Width / 24) * 24), Color.LightGray, Color.FromArgb(0x202020))
        MenuRender.WriteLine()

        MenuRender.WriteLine(MenuRender.Pad " - Upcoming workload (axis in weeks) - ", Color.LightGray, Color.FromArgb(0x303030))
        upcoming_bar(by_day, 500)
        upcoming_bar(by_day, 400)
        upcoming_bar(by_day, 300)
        upcoming_bar(by_day, 200)
        upcoming_bar(by_day, 100)
        for i = 0 to MenuRender.Width / 7 - 1 do
            MenuRender.Write((i + 1).ToString().PadLeft(7), Color.LightGray, Color.FromArgb(0x202020))
        MenuRender.Write("".PadLeft(MenuRender.Width - (MenuRender.Width / 7) * 7), Color.LightGray, Color.FromArgb(0x202020))
        MenuRender.WriteLine()

        if forgotten.Length > 0 then
            MenuRender.WriteLine(MenuRender.Pad " - Forgotten cards - ", Color.Red, Color.FromArgb(0x303030))
            for data, key in forgotten do
                MenuRender.Write((sprintf "[%i] %s" data.Level key).PadRight(MenuRender.Width - 44).Substring(0, MenuRender.Width - 44), ReviewData.LevelColors.[data.Level], Color.FromArgb(0x202020))
                MenuRender.Write($" {MenuRender.FormatInterval(now - data.LastReviewed)} ago ", Color.LightGray, Color.FromArgb(0x202020))
                MenuRender.Write($" Reviews: {data.Reviews.ToString().PadRight(3)} ", Color.Green, Color.FromArgb(0x202020))
                MenuRender.Write($" Difficulty {data.Difficulty.ToString().PadRight(2)} ", (if data.Difficulty >= 5 then Color.Red else Color.LightGray), Color.FromArgb(0x202020))
                MenuRender.WriteLine()

        MenuRender.FlushInline()
        Console.ReadKey(true) |> ignore
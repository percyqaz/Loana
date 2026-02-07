namespace Loana.Features

open System
open System.Drawing
open Loana.CLI
open Loana.Language
open Loana.Study
open Loana.GUI

type Chore =
    { Message: string; Urgent: bool }
    static member urgent message = { Message = message; Urgent = true }
    static member non_urgent message = { Message = message; Urgent = false }

type VocabDeck(scheduler: ReviewSchedule, words: WordBank) =

    member this.Scheduler = scheduler

    member inline this.LevelOf<^T when ^T : (member Key: string)>(c: ^T) : int =
        match this.Scheduler.Get c.Key with
        | ValueSome data -> data.Level
        | ValueNone -> 0

    member private this.AvailableCards(v: Vocab) : GuiCard seq =
        seq {
            let tier_1 = VocabCard.C_Tier1_RecogniseDE(v)
            yield tier_1

            if this.LevelOf tier_1 >= 2 then
                yield VocabCard.C_Tier2_RecallDE(v)
        }

    member private this.AvailableCards(n: Noun) : GuiCard seq =
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

    member private this.AvailableCards(word: WordlistItem): GuiCard seq =
        match word with
        | Vocab v -> this.AvailableCards v
        | Noun n -> this.AvailableCards n
        | Verb v -> this.AvailableCards v.Infinitive

    member this.AvailableCards(sources: string list) : GuiCard seq =
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
        | Verb v -> this.PossibleCards v.Infinitive

    member this.PossibleCards(sources: string list) : CardMeta seq =
        seq {
            for word in words.Entries do
                if sources.IsEmpty || List.contains word.Source.File sources then
                    yield! this.PossibleCards(word.Item)
        }
        |> Seq.cache

    member this.PossibleCards() = this.PossibleCards([])

    member this.FilterByTier(cards: GuiCard seq, min_tier: int, max_tier: int) =
        cards
        |> Seq.where (fun c -> c.Meta.Tier >= min_tier && c.Meta.Tier <= max_tier)

    member this.FilterByLevel(cards: GuiCard seq, minlevel: int, maxlevel: int) =
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
        |> Seq.where (fun c -> (this.Scheduler.Get c.Key).IsNone)

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

    member inline this.Stats<^T when ^T : (member Key: string)>(cards: ^T seq) : (int * int) seq =
        cards
        |> Seq.map this.LevelOf
        |> Seq.countBy id
        |> Seq.sortBy fst

    member this.Study() =

        let BAR_SIZE = 27
        let WIDTH = BAR_SIZE * 2 + 28 + 21

        App.StartThread()
        let mutable selected_group = []
        let groups =
            seq {
                for group in words.Groups do
                    yield List.ofSeq group.Lists
                    yield! group.Lists |> Seq.map List.singleton
                yield []
            }
            |> Array.ofSeq
        let next_selection() =
            selected_group <- groups.[(Array.IndexOf(groups, selected_group) + 1) % groups.Length]
        let previous_selection() =
            selected_group <- groups.[(Array.IndexOf(groups, selected_group) + groups.Length - 1) % groups.Length]
        let filters = [|
            id, "None";
            (fun cards -> this.FilterByTier(cards, 1, 1)), "New words only"
            (fun cards -> this.FilterByTier(cards, 2, 999)), "Unlocks only"
        |]
        let mutable current_filter = filters.[0]
        let cycle_filter() =
            current_filter <- filters.[(Array.IndexOf(filters, current_filter) + 1) % filters.Length]

        let pad_header(text: string) = text.PadLeft(WIDTH / 2 + text.Length / 2).PadRight(WIDTH)

        let get_filtered() =
            this.AvailableCards(selected_group) |> fst current_filter

        let review () =
            let cards = this.DueReviewCards(get_filtered(), DateTimeOffset.UtcNow.ToUnixTimeSeconds()) |> Seq.truncate 50 |> Array.ofSeq
            if cards.Length > 0 then
                Console.WriteLine(pad_header " Reviewing in progress! ", Color.Green, Color.FromArgb(0x303030))
                HtmlWindow.ShowUntilClosed(ReviewSession(cards, scheduler).Init)
            Console.ReadKey() |> ignore

        let review_ahead () =
            let cards = this.AheadReviewCards(get_filtered(), DateTimeOffset.UtcNow.ToUnixTimeSeconds()) |> Seq.truncate 50 |> Array.ofSeq
            if cards.Length > 0 then
                Console.WriteLine(pad_header " Reviewing (ahead) in progress! ", Color.Yellow, Color.FromArgb(0x303030))
                HtmlWindow.ShowUntilClosed(ReviewSession(cards, scheduler).Init)
            Console.ReadKey() |> ignore

        let learn () =
            let cards = this.LearningCards(get_filtered()) |> Seq.truncate 20 |> Array.ofSeq
            if cards.Length > 0 then
                Console.WriteLine(pad_header " Learning in progress! ", Color.LightBlue, Color.FromArgb(0x303030))
                HtmlWindow.ShowUntilClosed(LearnSession(cards, scheduler).Init)
            Console.ReadKey() |> ignore

        let chores () =
            Console.WriteLine(pad_header " Chores for selected deck(s) ", Color.White, Color.FromArgb(0x303030))
            let urgent = this.Chores() |> Seq.filter _.Urgent |> Seq.truncate 20 |> Array.ofSeq
            let non_urgent = this.Chores() |> Seq.filter (_.Urgent >> not) |> Seq.truncate 20 |> Array.ofSeq

            Console.WriteLine(pad_header (sprintf " - %i Urgent - " urgent.Length), Color.LightGray, Color.FromArgb(0x202020))
            for chore in urgent do Console.WriteLine(chore.Message, Color.DeepPink)
            Console.WriteLine(pad_header (sprintf " - %i Non-urgent - " non_urgent.Length), Color.LightGray, Color.FromArgb(0x202020))
            for chore in non_urgent do Console.WriteLine(chore.Message, Color.Yellow)

            Console.ReadKey() |> ignore

        let stats () =
            let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
            let all_cards = get_filtered()
            Console.WriteLine(pad_header " Stats for selected deck(s) ", Color.LightGray, Color.FromArgb(0x303030))
            Console.WriteLine(pad_header "- Distribution - ", Color.LightGray, Color.FromArgb(0x202020))
            all_cards
            |> this.Stats
            |> Seq.iter (fun (level, count) ->
                Console.Write(sprintf "[%i]" level, ReviewData.LevelColors.[level], Color.FromArgb(0x202020))
                Console.Write(String.replicate (count / 100) " ", Color.White, ReviewData.LevelColors.[level])
                Console.WriteLine((sprintf " %i cards" count).PadRight(WIDTH - (count / 100) - 3), Color.LightGray, Color.FromArgb(0x101010))
            )

            let by_hour =
                this.AheadReviewCards(all_cards, now)
                |> Seq.map (fun c -> (scheduler.Get c.Key).Value.NextReview - now)
                |> Seq.takeWhile (fun c -> c < TimeSpan.SecondsPerHour * int64 WIDTH)
                |> Seq.countBy (fun c -> c / TimeSpan.SecondsPerHour)
                |> Map.ofSeq

            let by_day =
                this.AheadReviewCards(all_cards, now)
                |> Seq.map (fun c -> (scheduler.Get c.Key).Value.NextReview - now)
                |> Seq.takeWhile (fun c -> c < TimeSpan.SecondsPerDay * int64 WIDTH)
                |> Seq.countBy (fun c -> c / TimeSpan.SecondsPerDay)
                |> Map.ofSeq

            let upcoming_bar (data, threshold) =
                for i = 0 to WIDTH - 1 do
                    let hit = (Map.tryFind (int64 i) data |> Option.defaultValue 0) > threshold
                    DeferConsole.Write(" ", Color.White, if hit then Color.Green else Color.FromArgb(0x101010))
                DeferConsole.WriteLine()
                DeferConsole.Flush()

            Console.WriteLine(pad_header "- Upcoming workload (axis in days) - ", Color.LightGray, Color.FromArgb(0x303030))
            upcoming_bar(by_hour, 250)
            upcoming_bar(by_hour, 200)
            upcoming_bar(by_hour, 150)
            upcoming_bar(by_hour, 100)
            upcoming_bar(by_hour, 50)

            for i = 0 to WIDTH / 24 - 1 do
                Console.Write("½".PadLeft(12), Color.LightGray, Color.FromArgb(0x202020))
                Console.Write((i + 1).ToString().PadLeft(12), Color.LightGray, Color.FromArgb(0x202020))
            Console.Write("".PadLeft(WIDTH - (WIDTH / 24) * 24), Color.LightGray, Color.FromArgb(0x202020))
            Console.WriteLine()

            Console.WriteLine(pad_header "- Upcoming workload (axis in weeks) - ", Color.LightGray, Color.FromArgb(0x303030))
            upcoming_bar(by_day, 500)
            upcoming_bar(by_day, 400)
            upcoming_bar(by_day, 300)
            upcoming_bar(by_day, 200)
            upcoming_bar(by_day, 100)
            for i = 0 to WIDTH / 7 - 1 do
                Console.Write((i + 1).ToString().PadLeft(7), Color.LightGray, Color.FromArgb(0x202020))
            Console.Write("".PadLeft(WIDTH - (WIDTH / 7) * 7), Color.LightGray, Color.FromArgb(0x202020))
            Console.WriteLine()

            Console.ReadKey() |> ignore
        let mutable loop = true

        while loop do

            let card_actions(word_lists: string list, is_group: bool) =
                let available = this.AvailableCards(word_lists) |> fst current_filter
                let learning = this.LearningCards(available)
                let due = this.DueReviewCards(available, DateTimeOffset.UtcNow.ToUnixTimeSeconds())
                let ahead = this.AheadReviewCards(available, DateTimeOffset.UtcNow.ToUnixTimeSeconds())

                let m = if is_group then 2 else 1

                DeferConsole.Write(sprintf " % 5i " (Seq.length learning), Color.LightBlue, Color.FromArgb(0x101020 * m))
                DeferConsole.Write(sprintf " % 5i " (Seq.length due), Color.Green, Color.FromArgb(0x102010 * m))
                DeferConsole.Write(sprintf " % 5i " (Seq.length ahead), Color.Yellow, Color.FromArgb(0x202010 * m))
                DeferConsole.Write(sprintf " % 5i " (Seq.length available), Color.White, Color.FromArgb(0x202020 * m))

            let progress_bar(word_lists: string list, is_group: bool) =
                let all_cards_ever = this.PossibleCards(word_lists)
                let total_cards = Seq.length all_cards_ever
                let started = total_cards - (this.LearningCards(all_cards_ever) |> Seq.length)
                let mature = all_cards_ever |> Seq.map this.LevelOf |> Seq.where (fun x -> x >= 4) |> Seq.length
                let mature_percent = float32 mature / float32 total_cards * 100.0f
                let started_percent = float32 started / float32 total_cards * 100.0f

                let m_c = mature_percent * float32 BAR_SIZE / 100.0f |> floor |> int
                let s_c = started_percent * float32 BAR_SIZE / 100.0f |> floor |> int
                let l_c = BAR_SIZE - s_c

                let m = if is_group then 2 else 1

                DeferConsole.Write("[", Color.FromArgb(0x606060 * m), Color.FromArgb(0x303030 * m))
                DeferConsole.Write(String.replicate m_c " ", Color.White, Color.Green)
                DeferConsole.Write(String.replicate (s_c - m_c) " ", Color.White, Color.FromArgb(0x30D070))
                DeferConsole.Write(String.replicate l_c " ", Color.White, Color.FromArgb(0x80AAFF))
                DeferConsole.Write("]", Color.FromArgb(0x606060 * m), Color.FromArgb(0x303030 * m))
                DeferConsole.Write((sprintf " %.1f%% " mature_percent).PadRight(8), Color.Green, Color.FromArgb(0x102010 * m))
                DeferConsole.Write(sprintf "| % 5i " total_cards, Color.White, Color.FromArgb(0x202020 * m))

            DeferConsole.Write(" The Word Eater :) ".PadRight(BAR_SIZE + 3), Color.White, Color.FromArgb(0x202020))
            DeferConsole.Write(
                $"  Filter: {snd current_filter} ".PadRight(28),
                (if snd current_filter = "None" then Color.LightGray else Color.DeepPink),
                Color.FromArgb(0x101010)
            )
            DeferConsole.Write((sprintf " %i chores " (Seq.length (this.Chores() |> Seq.filter _.Urgent))).PadLeft(BAR_SIZE + 18), Color.Pink, Color.FromArgb(0x202020))
            DeferConsole.WriteLine()
            for group in words.Groups do

                let word_lists = List.ofSeq group.Lists
                DeferConsole.Write(
                    $"@ {group.Name.PadRight(BAR_SIZE)} ",
                    (if selected_group = word_lists then Color.Yellow else Color.White),
                    Color.FromArgb(0x303030)
                )
                card_actions(word_lists, true)
                progress_bar(word_lists, true)
                DeferConsole.WriteLine()

                for wl in group.Lists do
                    DeferConsole.Write(
                        $"| {wl.PadRight(BAR_SIZE)} ",
                        (if selected_group = [wl] then Color.Yellow else Color.LightGreen),
                        Color.FromArgb(0x202020)
                    )
                    card_actions([wl], false)
                    progress_bar([wl], false)
                    DeferConsole.WriteLine()

            DeferConsole.Write(
                "** ALL CARDS ** ".PadRight(BAR_SIZE + 3),
                (if selected_group = [] then Color.Yellow else Color.White),
                Color.FromArgb(0x303030)
            )
            card_actions([], true)
            progress_bar([], true)
            DeferConsole.WriteLine()

            DeferConsole.Redraw()

            match Console.ReadKey(true).Key with
            | ConsoleKey.UpArrow -> previous_selection()
            | ConsoleKey.DownArrow -> next_selection()
            | ConsoleKey.Escape -> loop <- false
            | ConsoleKey.Enter -> stats()
            | ConsoleKey.L -> learn()
            | ConsoleKey.R -> review()
            | ConsoleKey.A -> review_ahead()
            | ConsoleKey.C -> chores()
            | ConsoleKey.F -> cycle_filter()
            | _ -> ()
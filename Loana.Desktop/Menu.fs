namespace Loana.Desktop

open System
open System.Drawing
open Loana.Language
open Loana.Data
open Loana.Vocab
open Loana.Verbs
open Loana.Desktop.Vocab
open Loana.Desktop.Verbs
open Loana.Desktop.Quizzes
open Loana.Desktop.CLI

type MenuSelection =
    | VocabGroup of string list
    | VerbMode
    | Quiz of Quiz

type Menu(words: WordBank, verb_cache: VerbBank, scheduler: ReviewSchedule) =

    let vocab: VocabDeck = VocabDeck(scheduler, words)
    let quizzes: QuizScheduler = QuizScheduler(scheduler)
    let verbs: VerbCache = VerbCache(scheduler, words)

    let SELECTION_OPTIONS =
        seq {
            for group in words.Groups do
                yield VocabGroup (List.ofSeq group.Lists)
                yield! group.Lists |> Seq.map List.singleton |> Seq.map VocabGroup
            yield VocabGroup []
            yield VerbMode
            for quiz in quizzes.Quizzes do
                yield Quiz quiz
        }
        |> Array.ofSeq

    let FILTERS = [|
        id, "None"
        (fun cards -> vocab.FilterByTier(cards, 1, 1)), "New words only"
        (fun cards -> vocab.FilterByTier(cards, 2, 999)), "Unlocks only"
    |]

    let mutable selection = VocabGroup []
    let mutable current_filter = FILTERS.[0]

    let next_selection() =
        selection <- SELECTION_OPTIONS.[(Array.IndexOf(SELECTION_OPTIONS, selection) + 1) % SELECTION_OPTIONS.Length]
    let previous_selection() =
        selection <- SELECTION_OPTIONS.[(Array.IndexOf(SELECTION_OPTIONS, selection) + SELECTION_OPTIONS.Length - 1) % SELECTION_OPTIONS.Length]

    let cycle_filter() =
        current_filter <- FILTERS.[(Array.IndexOf(FILTERS, current_filter) + 1) % FILTERS.Length]

    let get_filtered(wordlists: string list) =
        vocab.AvailableCards(wordlists) |> fst current_filter

    member private this.RenderVocabDashboard() =
        let BAR_SIZE = (MenuRender.Width - 28 - 21) / 2

        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()

        let card_actions(word_lists: string list, is_group: bool) =
            let available = vocab.AvailableCards(word_lists) |> fst current_filter
            let learning = vocab.LearningCards(available)
            let due = vocab.DueReviewCards(available, now)
            let ahead = vocab.AheadReviewCards(available, now)

            let m = if is_group then 2 else 1

            MenuRender.Write( $" %5i{Seq.length learning} ", Color.LightBlue, Color.FromArgb(0x01_101020 * m))
            MenuRender.Write( $" %5i{Seq.length due} ", Color.Green, Color.FromArgb(0x01_102010 * m))
            MenuRender.Write( $" %5i{Seq.length ahead} ", Color.Yellow, Color.FromArgb(0x01_202010 * m))
            MenuRender.Write( $" %5i{Seq.length available} ", Color.White, Color.FromArgb(0x01_202020 * m))

        let progress_bar(word_lists: string list, is_group: bool) =
            let all_cards_ever = vocab.PossibleCards(word_lists)
            let total_cards = Seq.length all_cards_ever
            let started = total_cards - (vocab.LearningCards(all_cards_ever) |> Seq.length)
            let mature = all_cards_ever |> Seq.map vocab.LevelOf |> Seq.where (fun x -> x >= 4) |> Seq.length
            let mature_percent = float32 mature / float32 total_cards * 100.0f
            let started_percent = float32 started / float32 total_cards * 100.0f

            let m_c = mature_percent * float32 BAR_SIZE / 100.0f |> floor |> int
            let s_c = started_percent * float32 BAR_SIZE / 100.0f |> floor |> int
            let l_c = BAR_SIZE - s_c

            let m = if is_group then 2 else 1

            MenuRender.Write("[", Color.FromArgb(0x01_606060 * m), Color.FromArgb(0x01_303030 * m))
            MenuRender.Write(String.replicate m_c " ", Color.White, Color.Green)
            MenuRender.Write(String.replicate (s_c - m_c) " ", Color.White, Color.FromArgb(0xFF_30D070))
            MenuRender.Write(String.replicate l_c " ", Color.White, Color.FromArgb(0xFF_80AAFF))
            MenuRender.Write("]", Color.FromArgb(0x606060 * m), Color.FromArgb(0x303030 * m))
            MenuRender.Write((sprintf " %.1f%% " mature_percent).PadRight(8), Color.Green, Color.FromArgb(0x01_102010 * m))
            MenuRender.Write(sprintf "| %5i " total_cards, Color.White, Color.FromArgb(0x01_202020 * m))

        MenuRender.Write(" Loana Dashboard :) ".PadRight(BAR_SIZE + 3), Color.White, Color.FromArgb(0xFF_202020))
        MenuRender.Write(
            $"  Filter: {snd current_filter} ".PadRight(28),
            (if snd current_filter = "None" then Color.LightGray else Color.DeepPink),
            Color.FromArgb(0xFF_101010)
        )
        MenuRender.Write("".PadLeft(BAR_SIZE - 8), Color.White, Color.FromArgb(0xFF_202020))
        MenuRender.Write( $" %3i{vocab.LearnBatchSize} ", Color.LightBlue, Color.FromArgb(0xFF_202040))
        MenuRender.Write( $" %3i{vocab.ReviewBatchSize} ", Color.Green, Color.FromArgb(0xFF_204020))
        MenuRender.Write( $" %i{Seq.length (vocab.Chores() |> Seq.filter _.Urgent)} chores ".PadLeft(16), Color.Pink, Color.FromArgb(0xFF_202020))
        MenuRender.WriteLine()
        for group in words.Groups do

            let word_lists = List.ofSeq group.Lists
            MenuRender.Write(
                $"@ {group.Name.PadRight(BAR_SIZE).Substring(0, BAR_SIZE)} ",
                (if selection = VocabGroup word_lists then Color.Yellow else Color.White),
                Color.FromArgb(0xFF_303030)
            )
            card_actions(word_lists, true)
            progress_bar(word_lists, true)
            MenuRender.WriteLine()

            for wl in group.Lists do
                MenuRender.Write(
                    $"| {wl.PadRight(BAR_SIZE).Substring(0, BAR_SIZE)} ",
                    (if selection = VocabGroup [wl] then Color.Yellow else Color.LightGreen),
                    Color.FromArgb(0xFF_202020)
                )
                card_actions([wl], false)
                progress_bar([wl], false)
                MenuRender.WriteLine()

        MenuRender.Write(
            "** ALL CARDS ** ".PadRight(BAR_SIZE + 3),
            (if selection = VocabGroup [] then Color.Yellow else Color.White),
            Color.FromArgb(0xFF_303030)
        )
        card_actions([], true)
        progress_bar([], true)
        MenuRender.WriteLine()

    member this.RenderVerbModeDashboard() =
        let BAR_SIZE = (MenuRender.Width - 28 - 21) / 2
        MenuRender.Write(
            "** VERB MODE ** ".PadRight(BAR_SIZE + 3),
            (if selection = VerbMode then Color.Yellow else Color.White),
            Color.FromArgb(0xFF_101010)
        )
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        let available = verbs.AvailableEntries()
        let learning = verbs.LearningEntries(available)
        let due = verbs.DueReviewEntries(available, now)
        let ahead = verbs.AheadReviewEntries(available, now)

        MenuRender.Write( $" %5i{Seq.length learning} ", Color.LightBlue, Color.FromArgb(0xFF_101020))
        MenuRender.Write( $" %5i{Seq.length due} ", Color.Green, Color.FromArgb(0xFF_102010))
        MenuRender.Write( $" %5i{Seq.length ahead} ", Color.Yellow, Color.FromArgb(0xFF_202010))
        MenuRender.Write( $" %5i{Seq.length available} ", Color.White, Color.FromArgb(0xFF_202020))

        MenuRender.Write(String.replicate (BAR_SIZE + 2) " ", Color.White, Color.FromArgb(0xFF_101010))
        MenuRender.Write(" ----- ".PadRight(8), Color.Green, Color.FromArgb(0xFF_102010))
        MenuRender.Write("| ----- ", Color.White, Color.FromArgb(0xFF_202020))
        MenuRender.WriteLine()

    member this.RenderQuizDashboard() =

        let BAR_SIZE = 45
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        let progress_bar(data: ReviewData voption) =
            match data with
            | ValueNone ->
                MenuRender.Write("[", Color.FromArgb(0xFF_606060), Color.FromArgb(0xFF_303030))
                MenuRender.Write(" N/A ".PadRight(BAR_SIZE), Color.LightGray, Color.FromArgb(0xFF_606060))
                MenuRender.Write("]", Color.FromArgb(0xFF_606060), Color.FromArgb(0xFF_303030))
            | ValueSome data ->
                let progress = if now > data.NextReview then 1.0f else float32 (now - data.LastReviewed) / float32 data.Interval
                let f_c = progress * float32 BAR_SIZE |> floor |> int
                let e_c = BAR_SIZE - f_c
                MenuRender.Write("[", Color.FromArgb(0xFF_606060), Color.FromArgb(0xFF_303030))
                MenuRender.Write(String.replicate f_c " ", Color.White, Color.Green)
                MenuRender.Write(String.replicate e_c " ", Color.White, Color.FromArgb(0xFF_303030))
                MenuRender.Write("]", Color.FromArgb(0xFF_606060), Color.FromArgb(0xFF_303030))

        for quiz in quizzes.Quizzes do
            let schedule = scheduler.Get quiz.Key
            let level = schedule |> ValueOption.map _.Level |> ValueOption.defaultValue 0
            let next_review = schedule |> ValueOption.map _.NextReview |> ValueOption.defaultValue now
            MenuRender.Write($"| {quiz.Name} ".PadRight(MenuRender.Width - 72), (if selection = Quiz quiz then Color.Yellow else Color.LightGreen), Color.FromArgb(0xFF_202020))
            MenuRender.Write($" Level %i{level} ", ReviewData.LevelColors.[level], Color.FromArgb(ReviewData.LevelColors.[level].ToArgb() / 2))
            progress_bar schedule
            if next_review <= now then
                MenuRender.Write(" DUE ".PadLeft(16), Color.Green, Color.FromArgb(0xFF_202020))
            else
                MenuRender.Write($" {MenuRender.FormatInterval(next_review - now)} ".PadLeft(16), Color.Yellow, Color.FromArgb(0xFF_202020))
            MenuRender.WriteLine()

    member this.VocabReview (cards: Card seq) =
        let cards =
            vocab.DueReviewCards(cards, DateTimeOffset.UtcNow.ToUnixTimeSeconds())
            |> Seq.distinctBy _.ReferenceKey
            |> Seq.truncate vocab.ReviewBatchSize
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
                Color.FromArgb(0xFF_303030)
            )
            Console.ReadKey(true) |> ignore

    member this.VocabReviewAhead (cards: Card seq) =
        let cards =
            vocab.AheadReviewCards(cards, DateTimeOffset.UtcNow.ToUnixTimeSeconds())
            |> Seq.distinctBy _.ReferenceKey
            |> Seq.truncate vocab.ReviewBatchSize
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
                Color.FromArgb(0xFF_303030)
            )
            Console.ReadKey(true) |> ignore

    member this.VocabLearn (cards: Card seq) =
        let cards = vocab.LearningCards(cards) |> Seq.truncate vocab.LearnBatchSize |> Array.ofSeq
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
                Color.FromArgb(0xFF_303030)
            )
            Console.ReadKey(true) |> ignore

    member this.VocabChoresList () =
        Console.WriteLine(MenuRender.Pad " Chores list ", Color.White, Color.FromArgb(0xFF_303030))
        let chores = vocab.Chores() |> Seq.cache
        let urgent = chores |> Seq.filter _.Urgent |> Seq.truncate 20 |> Array.ofSeq
        let non_urgent = chores |> Seq.filter (_.Urgent >> not) |> Seq.truncate 20 |> Array.ofSeq

        Console.WriteLine(MenuRender.Pad (sprintf " - %i Urgent - " urgent.Length), Color.LightGray, Color.FromArgb(0xFF_202020))
        for chore in urgent do Console.WriteLine(chore.Message, Color.DeepPink)
        Console.WriteLine(MenuRender.Pad (sprintf " - %i Non-urgent - " non_urgent.Length), Color.LightGray, Color.FromArgb(0xFF_202020))
        for chore in non_urgent do Console.WriteLine(chore.Message, Color.Yellow)

        Console.ReadKey(true) |> ignore

    member this.VocabStats(all_cards: Card seq) : unit =
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()

        let by_hour =
            vocab.AheadReviewCards(all_cards, now)
            |> Seq.map (fun c -> (scheduler.Get c.Key).Value.NextReview - now)
            |> Seq.takeWhile (fun c -> c < TimeSpan.SecondsPerHour * int64 MenuRender.Width)
            |> Seq.countBy (fun c -> c / TimeSpan.SecondsPerHour)
            |> Map.ofSeq

        let by_day =
            vocab.AheadReviewCards(all_cards, now)
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
                MenuRender.Write(" ", Color.White, if hit then Color.Green else Color.FromArgb(0xFF_101010))
            MenuRender.WriteLine()

        MenuRender.WriteLine(MenuRender.Pad " Stats for selected deck(s) ", Color.LightGray, Color.FromArgb(0xFF_303030))
        MenuRender.WriteLine(MenuRender.Pad " - Distribution - ", Color.LightGray, Color.FromArgb(0xFF_202020))
        all_cards
        |> vocab.LevelDistribution
        |> Seq.iter (fun (level, count) ->
            MenuRender.Write(sprintf "[%i]" level, ReviewData.LevelColors.[level], Color.FromArgb(ReviewData.LevelColors.[level].ToArgb() / 2))
            MenuRender.Write(String.replicate (count / 100) " ", Color.White, ReviewData.LevelColors.[level])
            MenuRender.WriteLine((sprintf " %i cards" count).PadRight(MenuRender.Width - (count / 100) - 3), Color.LightGray, Color.FromArgb(0xFF_101010))
        )

        MenuRender.WriteLine(MenuRender.Pad " - Upcoming workload (axis in days) - ", Color.LightGray, Color.FromArgb(0xFF_303030))
        upcoming_bar(by_hour, 250)
        upcoming_bar(by_hour, 200)
        upcoming_bar(by_hour, 150)
        upcoming_bar(by_hour, 100)
        upcoming_bar(by_hour, 50)

        for i = 0 to MenuRender.Width / 24 - 1 do
            MenuRender.Write("�".PadLeft(12), Color.LightGray, Color.FromArgb(0xFF_202020))
            MenuRender.Write((i + 1).ToString().PadLeft(12), Color.LightGray, Color.FromArgb(0xFF_202020))
        MenuRender.Write("".PadLeft(MenuRender.Width - (MenuRender.Width / 24) * 24), Color.LightGray, Color.FromArgb(0xFF_202020))
        MenuRender.WriteLine()

        MenuRender.WriteLine(MenuRender.Pad " - Upcoming workload (axis in weeks) - ", Color.LightGray, Color.FromArgb(0xFF_303030))
        upcoming_bar(by_day, 500)
        upcoming_bar(by_day, 400)
        upcoming_bar(by_day, 300)
        upcoming_bar(by_day, 200)
        upcoming_bar(by_day, 100)
        for i = 0 to MenuRender.Width / 7 - 1 do
            MenuRender.Write((i + 1).ToString().PadLeft(7), Color.LightGray, Color.FromArgb(0xFF_202020))
        MenuRender.Write("".PadLeft(MenuRender.Width - (MenuRender.Width / 7) * 7), Color.LightGray, Color.FromArgb(0xFF_202020))
        MenuRender.WriteLine()

        if forgotten.Length > 0 then
            MenuRender.WriteLine(MenuRender.Pad " - Forgotten cards - ", Color.Red, Color.FromArgb(0xFF_303030))
            for data, key in forgotten do
                MenuRender.Write((sprintf "[%i] %s" data.Level key).PadRight(MenuRender.Width - 44).Substring(0, MenuRender.Width - 44), ReviewData.LevelColors.[data.Level], Color.FromArgb(0xFF_202020))
                MenuRender.Write($" {MenuRender.FormatInterval(now - data.LastReviewed)} ago ", Color.LightGray, Color.FromArgb(0xFF_202020))
                MenuRender.Write($" Reviews: {data.Reviews.ToString().PadRight(3)} ", Color.Green, Color.FromArgb(0xFF_202020))
                MenuRender.Write($" Difficulty {data.Difficulty.ToString().PadRight(2)} ", (if data.Difficulty >= 5 then Color.Red else Color.LightGray), Color.FromArgb(0xFF_202020))
                MenuRender.WriteLine()

        MenuRender.FlushInline()
        Console.ReadKey(true) |> ignore

    member this.VerbsLearn (entries: VerbCacheEntry seq) =
        let to_learn =
            verbs.LearningEntries(entries)
            |> Seq.tryHead

        match to_learn with
        | None -> ()
        | Some verb ->
            let verb_cards =
                verb_cache.EnsureAllInflectionsAvailable(verb.Verb)
                |> Map.toSeq
                |> Seq.filter (fun (i, _) -> i.ToTense = verb.Tense)
                |> Seq.map (fun (i, text) -> VerbCard.M_Inflection(verb.Verb, i, text))
                |> Array.ofSeq

            let session = VerbSession(verb_cards)
            let result = session.Start()
            if not result.EndEarly then
                let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
                scheduler.Schedule(verb.Key, ReviewData.Level1(now, (1 + result.NotGood) |> min 10 |> max 1), now).LogTo session

        Console.WriteLine(MenuRender.Pad "Session ended.", Color.LightGreen, Color.FromArgb(0xFF_303030))
        Console.ReadKey(true) |> ignore

    member this.VerbsReview (entries: VerbCacheEntry seq) =
        let session_entries =
            verbs.DueReviewEntries(entries, DateTimeOffset.UtcNow.ToUnixTimeSeconds())
            |> Seq.truncate 5
            |> ResizeArray

        while session_entries.Count > 0 do
            let verb = session_entries.[0]
            session_entries.RemoveAt(0)

            let verb_cards =
                verb_cache.EnsureAllInflectionsAvailable(verb.Verb)
                |> Map.toSeq
                |> Seq.filter (fun (i, _) -> i.ToTense = verb.Tense)
                |> Seq.map (fun (i, text) -> VerbCard.M_Inflection(verb.Verb, i, text))
                |> Array.ofSeq

            let session = VerbSession(verb_cards)
            let result = session.Start()
            if result.EndEarly then
                session_entries.Clear()
            else
                if result.NotGood = 0 then
                    scheduler.Reschedule(verb.Key, _.Promote).LogTo session
                elif result.NotGood = 1 then
                    scheduler.Reschedule(verb.Key, _.Keep).LogTo session
                elif result.Forgot > 0 then
                    scheduler.Reschedule(verb.Key, _.Forget).LogTo session
                else
                    scheduler.Reschedule(verb.Key, _.Demote).LogTo session

        Console.WriteLine(MenuRender.Pad "Session ended.", Color.LightGreen, Color.FromArgb(0xFF_303030))
        Console.ReadKey(true) |> ignore

    member this.Run() : unit =
        let mutable loop = true
        while loop do
            MenuRender.UpdateWidth()
            this.RenderVocabDashboard()
            this.RenderVerbModeDashboard()
            this.RenderQuizDashboard()

            match selection with
            | VocabGroup wordlists ->
                MenuRender.WriteLine(MenuRender.Pad " [Enter] Stats  [L] Learn  [R] Review  [A] Review ahead  [C] Chores  [F] Filter ", Color.LightGray, Color.FromArgb(0xFF_303030))
                MenuRender.Redraw()

                match Console.ReadKey(true).Key with
                | ConsoleKey.Escape -> loop <- false
                | ConsoleKey.UpArrow | ConsoleKey.K -> previous_selection()
                | ConsoleKey.DownArrow | ConsoleKey.J -> next_selection()
                | ConsoleKey.Enter -> this.VocabStats(get_filtered(wordlists))
                | ConsoleKey.L -> this.VocabLearn(get_filtered(wordlists))
                | ConsoleKey.R -> this.VocabReview(get_filtered(wordlists))
                | ConsoleKey.A -> this.VocabReviewAhead(get_filtered(wordlists))
                | ConsoleKey.C -> this.VocabChoresList()
                | ConsoleKey.F -> cycle_filter()
                | ConsoleKey.OemMinus
                | ConsoleKey.Subtract -> vocab.DecreaseBatchSize()
                | ConsoleKey.OemPlus
                | ConsoleKey.Add -> vocab.IncreaseBatchSize()
                | ConsoleKey.S -> Sync.host(scheduler, words); Console.ReadKey(true) |> ignore
                | _ -> ()

            | VerbMode ->
                MenuRender.WriteLine(MenuRender.Pad " [L] Learn  [R] Review ", Color.LightGray, Color.FromArgb(0xFF_303030))
                MenuRender.Redraw()

                match Console.ReadKey(true).Key with
                | ConsoleKey.Escape -> loop <- false
                | ConsoleKey.UpArrow | ConsoleKey.K -> previous_selection()
                | ConsoleKey.DownArrow | ConsoleKey.J -> next_selection()
                | ConsoleKey.L -> this.VerbsLearn(verbs.LearningEntries(verbs.AvailableEntries()))
                | ConsoleKey.R -> this.VerbsReview(verbs.DueReviewEntries(verbs.AvailableEntries(), DateTimeOffset.UtcNow.ToUnixTimeSeconds()))
                | _ -> ()

            | Quiz quiz ->
                MenuRender.WriteLine(MenuRender.Pad " [Enter] Quiz  [A] Auto ", Color.LightGray, Color.FromArgb(0xFF_303030))
                MenuRender.Redraw()

                match Console.ReadKey(true).Key with
                | ConsoleKey.Escape -> loop <- false
                | ConsoleKey.UpArrow | ConsoleKey.K -> previous_selection()
                | ConsoleKey.DownArrow | ConsoleKey.J -> next_selection()
                | ConsoleKey.Enter -> quizzes.Study(quiz)
                | ConsoleKey.A -> quizzes.Study(quizzes.Auto())
                | _ -> ()
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

type MenuView(state: MenuState) =

    member private this.RenderVocabDashboard() : unit =
        let BAR_SIZE = (MenuRender.Width - 28 - 21) / 2

        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()

        let card_actions (word_lists: string list, is_group: bool) =
            let available = state.FilteredWords(word_lists)
            let learning = state.Vocab.LearningCards(available)
            let due = state.Vocab.DueReviewCards(available, now)
            let ahead = state.Vocab.AheadReviewCards(available, now)

            let m = if is_group then 2 else 1

            MenuRender.Write($" %5i{Seq.length learning} ", Color.LightBlue, Color.FromArgb(0x01_101020 * m))
            MenuRender.Write($" %5i{Seq.length due} ", Color.Green, Color.FromArgb(0x01_102010 * m))
            MenuRender.Write($" %5i{Seq.length ahead} ", Color.Yellow, Color.FromArgb(0x01_202010 * m))
            MenuRender.Write($" %5i{Seq.length available} ", Color.White, Color.FromArgb(0x01_202020 * m))

        let progress_bar (word_lists: string list, is_group: bool) =
            let all_cards_ever = state.Vocab.PossibleCards(word_lists)
            let total_cards = Seq.length all_cards_ever

            let started =
                total_cards - (state.Vocab.LearningCards(all_cards_ever) |> Seq.length)

            let mature =
                all_cards_ever |> Seq.map state.Vocab.LevelOf |> Seq.where(fun x -> x >= 4) |> Seq.length

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

            MenuRender.Write(
                (sprintf " %.1f%% " mature_percent).PadRight(8),
                Color.Green,
                Color.FromArgb(0x01_102010 * m)
            )

            MenuRender.Write(sprintf "| %5i " total_cards, Color.White, Color.FromArgb(0x01_202020 * m))

        MenuRender.Write(" Loana Dashboard :) ".PadRight(BAR_SIZE + 3), Color.White, Color.FromArgb(0xFF_202020))

        MenuRender.Write(
            $"  Filter: {state.Filter.Name} ".PadRight(28),
            (if state.Filter.Name = "None" then Color.LightGray else Color.DeepPink),
            Color.FromArgb(0xFF_101010)
        )

        MenuRender.Write("".PadLeft(BAR_SIZE - 8), Color.White, Color.FromArgb(0xFF_202020))
        MenuRender.Write($" %3i{state.LearnBatchSize} ", Color.LightBlue, Color.FromArgb(0xFF_202040))
        MenuRender.Write($" %3i{state.ReviewBatchSize} ", Color.Green, Color.FromArgb(0xFF_204020))

        MenuRender.Write(
            $" %i{Seq.length(state.Vocab.Chores() |> Seq.filter _.IsUrgent)} chores ".PadLeft(16),
            Color.Pink,
            Color.FromArgb(0xFF_202020)
        )

        MenuRender.WriteLine()

        for group in state.Words.Groups do

            let word_lists = List.ofSeq group.WordlistNames

            MenuRender.Write(
                $"@ {group.Name.PadRight(BAR_SIZE).Substring(0, BAR_SIZE)} ",
                (if state.Selection = VocabGroup word_lists then Color.Yellow else Color.White),
                Color.FromArgb(0xFF_303030)
            )

            card_actions(word_lists, true)
            progress_bar(word_lists, true)
            MenuRender.WriteLine()

            for wl in group.WordlistNames do
                MenuRender.Write(
                    $"| {wl.PadRight(BAR_SIZE).Substring(0, BAR_SIZE)} ",
                    (if state.Selection = VocabGroup [ wl ] then Color.Yellow else Color.LightGreen),
                    Color.FromArgb(0xFF_202020)
                )

                card_actions([ wl ], false)
                progress_bar([ wl ], false)
                MenuRender.WriteLine()

        MenuRender.Write(
            "** ALL CARDS ** ".PadRight(BAR_SIZE + 3),
            (if state.Selection = VocabGroup [] then Color.Yellow else Color.White),
            Color.FromArgb(0xFF_303030)
        )

        card_actions([], true)
        progress_bar([], true)
        MenuRender.WriteLine()

    member this.RenderVerbModeDashboard() : unit =
        let BAR_SIZE = (MenuRender.Width - 28 - 21) / 2

        MenuRender.Write(
            "** VERB MODE ** ".PadRight(BAR_SIZE + 3),
            (if state.Selection = VerbMode then Color.Yellow else Color.White),
            Color.FromArgb(0xFF_101010)
        )

        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        let available = state.Verbs.AvailableEntries()
        let learning = state.Verbs.LearningEntries(available)
        let due = state.Verbs.DueReviewEntries(available, now)
        let ahead = state.Verbs.AheadReviewEntries(available, now)

        MenuRender.Write($" %5i{Seq.length learning} ", Color.LightBlue, Color.FromArgb(0xFF_101020))
        MenuRender.Write($" %5i{Seq.length due} ", Color.Green, Color.FromArgb(0xFF_102010))
        MenuRender.Write($" %5i{Seq.length ahead} ", Color.Yellow, Color.FromArgb(0xFF_202010))
        MenuRender.Write($" %5i{Seq.length available} ", Color.White, Color.FromArgb(0xFF_202020))

        MenuRender.Write(String.replicate (BAR_SIZE + 2) " ", Color.White, Color.FromArgb(0xFF_101010))
        MenuRender.Write(" ----- ".PadRight(8), Color.Green, Color.FromArgb(0xFF_102010))
        MenuRender.Write("| ----- ", Color.White, Color.FromArgb(0xFF_202020))
        MenuRender.WriteLine()

    member this.RenderQuizDashboard() : unit =

        let BAR_SIZE = 45
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()

        let progress_bar (data: ReviewData voption) =
            match data with
            | ValueNone ->
                MenuRender.Write("[", Color.FromArgb(0xFF_606060), Color.FromArgb(0xFF_303030))
                MenuRender.Write(" N/A ".PadRight(BAR_SIZE), Color.LightGray, Color.FromArgb(0xFF_606060))
                MenuRender.Write("]", Color.FromArgb(0xFF_606060), Color.FromArgb(0xFF_303030))
            | ValueSome data ->
                let progress =
                    if now > data.NextReview then 1.0f else float32(now - data.LastReviewed) / float32 data.Interval

                let f_c = progress * float32 BAR_SIZE |> floor |> int
                let e_c = BAR_SIZE - f_c
                MenuRender.Write("[", Color.FromArgb(0xFF_606060), Color.FromArgb(0xFF_303030))
                MenuRender.Write(String.replicate f_c " ", Color.White, Color.Green)
                MenuRender.Write(String.replicate e_c " ", Color.White, Color.FromArgb(0xFF_303030))
                MenuRender.Write("]", Color.FromArgb(0xFF_606060), Color.FromArgb(0xFF_303030))

        for quiz in state.Quizzes.Quizzes do
            let schedule = state.Scheduler.Get(quiz.Key)
            let level = schedule |> ValueOption.map _.Level |> ValueOption.defaultValue 0

            let next_review =
                schedule |> ValueOption.map _.NextReview |> ValueOption.defaultValue now

            MenuRender.Write(
                $"| {quiz.Name} ".PadRight(MenuRender.Width - 72),
                (if state.Selection = Quiz quiz then Color.Yellow else Color.LightGreen),
                Color.FromArgb(0xFF_202020)
            )

            MenuRender.Write(
                $" Level %i{level} ",
                ReviewData.LevelColors.[level],
                Color.FromArgb(ReviewData.LevelColors.[level].ToArgb() / 2)
            )

            progress_bar schedule

            if next_review <= now then
                MenuRender.Write(" DUE ".PadLeft(16), Color.Green, Color.FromArgb(0xFF_202020))
            else
                MenuRender.Write(
                    $" {MenuRender.FormatInterval(next_review - now)} ".PadLeft(16),
                    Color.Yellow,
                    Color.FromArgb(0xFF_202020)
                )

            MenuRender.WriteLine()

    member this.VocabReview(cards: Card seq) : unit =
        let cards =
            state.Vocab.DueReviewCards(cards, DateTimeOffset.UtcNow.ToUnixTimeSeconds())
            |> Seq.distinctBy _.ReferenceKey
            |> Seq.truncate state.ReviewBatchSize
            |> Array.ofSeq

        if cards.Length > 0 then
            let result = ReviewSession(cards, state.Scheduler, false).Start()

            Console.WriteLine(
                MenuRender
                    .Pad(
                        sprintf
                            "Session ended%s! [%i|%i|%i|%i] (%.1f%%)"
                            (if result.EndEarly then " early" else "")
                            result.Good
                            result.Ok
                            result.Bad
                            result.NotGood
                            (100.0f
                             * (float32 result.Good / ((float32 result.Good + float32 result.NotGood) |> max 1.0f)))
                    )
                    .ForeColor(Color.LightGreen)
                    .BackColor(Color.FromArgb(0xFF_303030))
            )

            Console.ReadKey(true) |> ignore

    member this.VocabReviewAhead(cards: Card seq) : unit =
        let cards =
            state.Vocab.AheadReviewCards(cards, DateTimeOffset.UtcNow.ToUnixTimeSeconds())
            |> Seq.distinctBy _.ReferenceKey
            |> Seq.truncate state.ReviewBatchSize
            |> Array.ofSeq

        if cards.Length > 0 then
            let result = ReviewSession(cards, state.Scheduler, true).Start()

            Console.WriteLine(
                MenuRender
                    .Pad(
                        sprintf
                            "Session ended%s! [%i|%i|%i|%i] (%.1f%%)"
                            (if result.EndEarly then " early" else "")
                            result.Good
                            result.Ok
                            result.Bad
                            result.NotGood
                            (100.0f
                             * (float32 result.Good / ((float32 result.Good + float32 result.NotGood) |> max 1.0f)))
                    )
                    .ForeColor(Color.LightGreen)
                    .BackColor(Color.FromArgb(0xFF_303030))
            )

            Console.ReadKey(true) |> ignore

    member this.VocabLearn(cards: Card seq) : unit =
        let cards =
            state.Vocab.LearningCards(cards) |> Seq.truncate state.LearnBatchSize |> Array.ofSeq

        if cards.Length > 0 then
            let result = LearnSession(cards, state.Scheduler).Start()

            Console.WriteLine(
                MenuRender
                    .Pad(
                        sprintf
                            "Session ended%s! [%i|%i] (%.1f)"
                            (if result.EndEarly then " early" else "")
                            result.Good
                            result.NotGood
                            (1.0f + float32 result.NotGood / (float32 result.Good |> max 1.0f))
                    )
                    .ForeColor(Color.LightGreen)
                    .BackColor(Color.FromArgb(0xFF_303030))
            )

            Console.ReadKey(true) |> ignore

    member this.VocabChoresList() : unit =
        Console.WriteLine(MenuRender.Pad(" Chores list ").ForeColor(Color.White).BackColor(Color.FromArgb(0xFF_303030)))

        let chores = state.Vocab.Chores() |> Seq.cache
        let urgent = chores |> Seq.filter _.IsUrgent |> Seq.truncate 20 |> Array.ofSeq

        let non_urgent =
            chores |> Seq.filter(_.IsUrgent >> not) |> Seq.truncate 20 |> Array.ofSeq

        Console.WriteLine(
            MenuRender.Pad(sprintf " - %i Urgent - " urgent.Length).ForeColor(Color.LightGray).BackColor(0xFF_202020)
        )

        for chore in urgent do
            Console.WriteLine(chore.Message.ForeColor(Color.DeepPink))

        Console.WriteLine(
            MenuRender
                .Pad(sprintf " - %i Non-urgent - " non_urgent.Length)
                .ForeColor(Color.LightGray)
                .BackColor(0xFF_202020)
        )

        for chore in non_urgent do
            Console.WriteLine(chore.Message.ForeColor(Color.Yellow))

        Console.ReadKey(true) |> ignore

    member this.VocabStats(all_cards: Card seq) : unit =
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()

        let by_hour =
            state.Vocab.AheadReviewCards(all_cards, now)
            |> Seq.map(fun c -> state.Scheduler.Get(c.Key).Value.NextReview - now)
            |> Seq.takeWhile(fun c -> c < TimeSpan.SecondsPerHour * int64 MenuRender.Width)
            |> Seq.countBy(fun c -> c / TimeSpan.SecondsPerHour)
            |> Map.ofSeq

        let by_day =
            state.Vocab.AheadReviewCards(all_cards, now)
            |> Seq.map(fun c -> state.Scheduler.Get(c.Key).Value.NextReview - now)
            |> Seq.takeWhile(fun c -> c < TimeSpan.SecondsPerDay * int64 MenuRender.Width)
            |> Seq.countBy(fun c -> c / TimeSpan.SecondsPerDay)
            |> Map.ofSeq

        let forgotten =
            all_cards
            |> Seq.choose(fun c ->
                match state.Scheduler.Get(c.Key) with
                | ValueSome v when (v.Reviews > v.Level && v.Level < 4) || v.Difficulty > 5 -> Some(v, c.Key)
                | _ -> None
            )
            |> Seq.sortByDescending(fst >> _.LastReviewed)
            |> Seq.truncate 20
            |> Array.ofSeq

        let upcoming_bar (data, threshold) =
            for i = 0 to MenuRender.Width - 1 do
                let hit = (Map.tryFind (int64 i) data |> Option.defaultValue 0) > threshold
                MenuRender.Write(" ", Color.White, (if hit then Color.Green else Color.FromArgb(0xFF_101010)))

            MenuRender.WriteLine()

        MenuRender.WriteLine(
            MenuRender.Pad(" Stats for selected deck(s) "),
            Color.LightGray,
            Color.FromArgb(0xFF_303030)
        )

        MenuRender.WriteLine(MenuRender.Pad(" - Distribution - "), Color.LightGray, Color.FromArgb(0xFF_202020))

        all_cards
        |> state.Vocab.LevelDistribution
        |> Seq.iter(fun (level, count) ->
            MenuRender.Write(
                sprintf "[%i]" level,
                ReviewData.LevelColors.[level],
                Color.FromArgb(ReviewData.LevelColors.[level].ToArgb() / 2)
            )

            MenuRender.Write(String.replicate (count / 100) " ", Color.White, ReviewData.LevelColors.[level])

            MenuRender.WriteLine(
                (sprintf " %i cards" count).PadRight(MenuRender.Width - (count / 100) - 3),
                Color.LightGray,
                Color.FromArgb(0xFF_101010)
            )
        )

        MenuRender.WriteLine(
            MenuRender.Pad(" - Upcoming workload (axis in days) - "),
            Color.LightGray,
            Color.FromArgb(0xFF_303030)
        )

        upcoming_bar(by_hour, 250)
        upcoming_bar(by_hour, 200)
        upcoming_bar(by_hour, 150)
        upcoming_bar(by_hour, 100)
        upcoming_bar(by_hour, 50)

        for i = 0 to MenuRender.Width / 24 - 1 do
            MenuRender.Write("�".PadLeft(12), Color.LightGray, Color.FromArgb(0xFF_202020))
            MenuRender.Write((i + 1).ToString().PadLeft(12), Color.LightGray, Color.FromArgb(0xFF_202020))

        MenuRender.Write(
            "".PadLeft(MenuRender.Width - (MenuRender.Width / 24) * 24),
            Color.LightGray,
            Color.FromArgb(0xFF_202020)
        )

        MenuRender.WriteLine()

        MenuRender.WriteLine(
            MenuRender.Pad(" - Upcoming workload (axis in weeks) - "),
            Color.LightGray,
            Color.FromArgb(0xFF_303030)
        )

        upcoming_bar(by_day, 500)
        upcoming_bar(by_day, 400)
        upcoming_bar(by_day, 300)
        upcoming_bar(by_day, 200)
        upcoming_bar(by_day, 100)

        for i = 0 to MenuRender.Width / 7 - 1 do
            MenuRender.Write((i + 1).ToString().PadLeft(7), Color.LightGray, Color.FromArgb(0xFF_202020))

        MenuRender.Write(
            "".PadLeft(MenuRender.Width - (MenuRender.Width / 7) * 7),
            Color.LightGray,
            Color.FromArgb(0xFF_202020)
        )

        MenuRender.WriteLine()

        if forgotten.Length > 0 then
            MenuRender.WriteLine(MenuRender.Pad(" - Forgotten cards - "), Color.Red, Color.FromArgb(0xFF_303030))

            for data, key in forgotten do
                MenuRender.Write(
                    (sprintf "[%i] %s" data.Level key)
                        .PadRight(MenuRender.Width - 44)
                        .Substring(0, MenuRender.Width - 44),
                    ReviewData.LevelColors.[data.Level],
                    Color.FromArgb(0xFF_202020)
                )

                MenuRender.Write(
                    $" {MenuRender.FormatInterval(now - data.LastReviewed)} ago ",
                    Color.LightGray,
                    Color.FromArgb(0xFF_202020)
                )

                MenuRender.Write(
                    $" Reviews: {data.Reviews.ToString().PadRight(3)} ",
                    Color.Green,
                    Color.FromArgb(0xFF_202020)
                )

                MenuRender.Write(
                    $" Difficulty {data.Difficulty.ToString().PadRight(2)} ",
                    (if data.Difficulty >= 5 then Color.Red else Color.LightGray),
                    Color.FromArgb(0xFF_202020)
                )

                MenuRender.WriteLine()

        MenuRender.FlushInline()
        Console.ReadKey(true) |> ignore

    member this.VerbsLearn(entries: VerbCacheEntry seq) : unit =
        let to_learn = state.Verbs.LearningEntries(entries) |> Seq.tryHead

        match to_learn with
        | None -> ()
        | Some verb ->
            let verb_cards =
                state.Data.Verbs.EnsureAllInflectionsAvailable(verb.Verb)
                |> Map.toSeq
                |> Seq.filter(fun (i, _) -> i.ToTense = verb.Tense)
                |> Seq.map(fun (i, text) -> VerbCard.Inflection(verb.Verb, i, text))
                |> Array.ofSeq

            let session = VerbSession(verb_cards)
            let result = session.Start()

            if not result.EndEarly then
                let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()

                state.Scheduler
                    .Schedule(verb.Key, ReviewData.Level1(now, (1 + result.NotGood) |> min 10 |> max 1), now)
                    .LogTo
                    session

        Console.WriteLine(
            MenuRender.Pad("Session ended.").ForeColor(Color.LightGreen).BackColor(Color.FromArgb(0xFF_303030))
        )

        Console.ReadKey(true) |> ignore

    member this.VerbsReview(entries: VerbCacheEntry seq) : unit =
        let session_entries =
            state.Verbs.DueReviewEntries(entries, DateTimeOffset.UtcNow.ToUnixTimeSeconds())
            |> Seq.truncate 5
            |> ResizeArray

        while session_entries.Count > 0 do
            let verb = session_entries.[0]
            session_entries.RemoveAt(0)

            let verb_cards =
                state.Data.Verbs.EnsureAllInflectionsAvailable(verb.Verb)
                |> Map.toSeq
                |> Seq.filter(fun (i, _) -> i.ToTense = verb.Tense)
                |> Seq.map(fun (i, text) -> VerbCard.Inflection(verb.Verb, i, text))
                |> Array.ofSeq

            let session = VerbSession(verb_cards)
            let result = session.Start()

            if result.EndEarly then session_entries.Clear()
            elif result.NotGood = 0 then state.Scheduler.Reschedule(verb.Key, _.Promote).LogTo session
            elif result.NotGood = 1 then state.Scheduler.Reschedule(verb.Key, _.Keep).LogTo session
            elif result.Forgot > 0 then state.Scheduler.Reschedule(verb.Key, _.Forget).LogTo session
            else state.Scheduler.Reschedule(verb.Key, _.Demote).LogTo session

        Console.WriteLine(
            MenuRender.Pad("Session ended.").ForeColor(Color.LightGreen).BackColor(Color.FromArgb(0xFF_303030))
        )

        Console.ReadKey(true) |> ignore

    member this.Run() : unit =
        let mutable loop = true

        while loop do
            MenuRender.UpdateWidth()
            this.RenderVocabDashboard()
            this.RenderVerbModeDashboard()
            this.RenderQuizDashboard()

            match state.Selection with
            | VocabGroup wordlists ->
                MenuRender.WriteLine(
                    MenuRender.Pad(" [Enter] Stats  [L] Learn  [R] Review  [A] Review ahead  [C] Chores  [F] Filter "),
                    Color.LightGray,
                    Color.FromArgb(0xFF_303030)
                )

                MenuRender.Redraw()

                match Console.ReadKey(true).Key with
                | ConsoleKey.Escape -> loop <- false
                | ConsoleKey.UpArrow
                | ConsoleKey.K -> state.PreviousSelection()
                | ConsoleKey.DownArrow
                | ConsoleKey.J -> state.NextSelection()
                | ConsoleKey.Enter -> this.VocabStats(state.FilteredWords(wordlists))
                | ConsoleKey.L -> this.VocabLearn(state.FilteredWords(wordlists))
                | ConsoleKey.R -> this.VocabReview(state.FilteredWords(wordlists))
                | ConsoleKey.A -> this.VocabReviewAhead(state.FilteredWords(wordlists))
                | ConsoleKey.C -> this.VocabChoresList()
                | ConsoleKey.F -> state.CycleFilter()
                | ConsoleKey.OemMinus
                | ConsoleKey.Subtract -> state.DecreaseBatchSize()
                | ConsoleKey.OemPlus
                | ConsoleKey.Add -> state.IncreaseBatchSize()
                | ConsoleKey.S ->
                    Sync.host(state.Data)
                    Console.ReadKey(true) |> ignore
                | _ -> ()

            | VerbMode ->
                MenuRender.WriteLine(
                    MenuRender.Pad(" [L] Learn  [R] Review "),
                    Color.LightGray,
                    Color.FromArgb(0xFF_303030)
                )

                MenuRender.Redraw()

                match Console.ReadKey(true).Key with
                | ConsoleKey.Escape -> loop <- false
                | ConsoleKey.UpArrow
                | ConsoleKey.K -> state.PreviousSelection()
                | ConsoleKey.DownArrow
                | ConsoleKey.J -> state.NextSelection()
                | ConsoleKey.L -> this.VerbsLearn(state.Verbs.LearningEntries(state.Verbs.AvailableEntries()))
                | ConsoleKey.R ->
                    this.VerbsReview(
                        state.Verbs.DueReviewEntries(
                            state.Verbs.AvailableEntries(),
                            DateTimeOffset.UtcNow.ToUnixTimeSeconds()
                        )
                    )
                | _ -> ()

            | Quiz quiz ->
                MenuRender.WriteLine(
                    MenuRender.Pad(" [Enter] Quiz  [A] Auto "),
                    Color.LightGray,
                    Color.FromArgb(0xFF_303030)
                )

                MenuRender.Redraw()

                match Console.ReadKey(true).Key with
                | ConsoleKey.Escape -> loop <- false
                | ConsoleKey.UpArrow
                | ConsoleKey.K -> state.PreviousSelection()
                | ConsoleKey.DownArrow
                | ConsoleKey.J -> state.NextSelection()
                | ConsoleKey.Enter -> state.Quizzes.Study(quiz)
                | ConsoleKey.A -> state.Quizzes.Study(state.Quizzes.Auto())
                | _ -> ()

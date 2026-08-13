namespace Loana.Desktop

open System
open System.Drawing
open System.Runtime.CompilerServices
open Loana.Desktop.Browser
open Loana.Language
open Loana.Data
open Loana.Vocab
open Loana.Verbs
open Loana.Desktop.Vocab
open Loana.Desktop.Verbs
open Loana.Desktop.CLI
open Loana.Desktop.Study

type MenuCommands =

    [<Extension>]
    static member Exit(state: MenuState) : unit = state.Running <- false

    [<Extension>]
    static member Echo(state: MenuState, text: string) : unit = state.UIContext.StatusLine <- text

    // todo: reload command

    [<Extension>]
    static member Sync(state: MenuState) : unit =
        Sync.host(state.Data)
        Console.ReadKey(true) |> ignore

    [<Extension>]
    static member Browse(state: MenuState) : unit = WordBrowser(state.Words).Run()

    [<Extension>]
    static member NextSelection(state: MenuState) : unit =
        let new_index =
            (Array.IndexOf(state.SelectionOptions, state.Selection) + 1) % state.SelectionOptions.Length

        state.Selection <- state.SelectionOptions.[new_index]

    [<Extension>]
    static member PreviousSelection(state: MenuState) : unit =
        let new_index =
            (Array.IndexOf(state.SelectionOptions, state.Selection) + state.SelectionOptions.Length - 1) % state.SelectionOptions.Length

        state.Selection <- state.SelectionOptions.[new_index]

    [<Extension>]
    static member CycleFilter(state: MenuState) : unit =
        let new_index =
            (Array.IndexOf(MenuFilter.Options, state.Filter) + 1) % MenuFilter.Options.Length

        state.Filter <- MenuFilter.Options.[new_index]

    [<Extension>]
    static member IncreaseBatchSize(state: MenuState) : unit =
        state.BatchSize <- state.BatchSize + 1 |> min 20

    [<Extension>]
    static member DecreaseBatchSize(state: MenuState) : unit =
        state.BatchSize <- state.BatchSize - 1 |> max 1

    [<Extension>]
    static member Review(state: MenuState) : unit =
        match state.Selection with
        | VocabGroup word_lists ->
            let cards =
                state.Vocab.DueReviewCards(state.FilteredWords(word_lists), DateTimeOffset.UtcNow.ToUnixTimeSeconds())
                |> Seq.distinctBy _.ReferenceKey
                |> Seq.truncate state.ReviewBatchSize
                |> Array.ofSeq

            if cards.Length > 0 then
                let result =
                    ReviewSession(StudySessionState.Review(cards, state.UIContext), state.Scheduler).Run()

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
        | VerbMode ->
            let session_entries =
                state.Verbs.DueReviewEntries(state.Verbs.AvailableEntries(), DateTimeOffset.UtcNow.ToUnixTimeSeconds())
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

                let session = VerbSession(StudySessionState.VerbMode(verb_cards, state.UIContext))
                let result = session.Run()

                if result.EndEarly then session_entries.Clear()
                elif result.NotGood = 0 then state.Scheduler.Reschedule(verb.Key, _.Promote).LogTo(session)
                elif result.NotGood = 1 then state.Scheduler.Reschedule(verb.Key, _.Keep).LogTo(session)
                elif result.Forgot > 0 then state.Scheduler.Reschedule(verb.Key, _.Forget).LogTo(session)
                else state.Scheduler.Reschedule(verb.Key, _.Demote).LogTo(session)

            Console.WriteLine(
                MenuRender.Pad("Session ended.").ForeColor(Color.LightGreen).BackColor(Color.FromArgb(0xFF_303030))
            )

            Console.ReadKey(true) |> ignore
        | Quiz quiz -> state.Quizzes.Study(quiz)

    [<Extension>]
    static member VocabReviewAhead(state: MenuState) : unit =
        match state.Selection with
        | VocabGroup word_lists ->
            let cards =
                state.Vocab.AheadReviewCards(state.FilteredWords(word_lists), DateTimeOffset.UtcNow.ToUnixTimeSeconds())
                |> Seq.distinctBy _.ReferenceKey
                |> Seq.truncate state.ReviewBatchSize
                |> Array.ofSeq

            if cards.Length > 0 then
                let result =
                    ReviewSession(StudySessionState.Review(cards, state.UIContext), state.Scheduler).Run()

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
        | _ -> ()

    [<Extension>]
    static member Learn(state: MenuState) : unit =
        match state.Selection with
        | VocabGroup word_lists ->
            let cards =
                state.FilteredWords(word_lists)
                |> state.Vocab.LearningCards
                |> Seq.truncate(state.LearnBatchSize)
                |> Array.ofSeq

            if cards.Length > 0 then
                let result =
                    LearnSession(StudySessionState.Learn(cards, state.UIContext), state.Scheduler).Run()

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
        | VerbMode ->
            let to_learn =
                state.Verbs.AvailableEntries() |> state.Verbs.LearningEntries |> Seq.tryHead

            match to_learn with
            | None -> ()
            | Some verb ->
                let verb_cards =
                    state.Data.Verbs.EnsureAllInflectionsAvailable(verb.Verb)
                    |> Map.toSeq
                    |> Seq.filter(fun (i, _) -> i.ToTense = verb.Tense)
                    |> Seq.map(fun (i, text) -> VerbCard.Inflection(verb.Verb, i, text))
                    |> Array.ofSeq

                let session = VerbSession(StudySessionState.VerbMode(verb_cards, state.UIContext))
                let result = session.Run()

                if not result.EndEarly then
                    let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()

                    state.Scheduler
                        .Schedule(verb.Key, ReviewData.Level1(now, (1 + result.NotGood) |> min 10 |> max 1), now)
                        .LogTo(session)

            Console.WriteLine(
                MenuRender.Pad("Session ended.").ForeColor(Color.LightGreen).BackColor(Color.FromArgb(0xFF_303030))
            )

            Console.ReadKey(true) |> ignore
        | Quiz quiz -> state.Quizzes.Study(quiz)

    [<Extension>]
    static member VocabChoresList(state: MenuState) : unit =
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

    [<Extension>]
    static member VocabStats(state: MenuState) : unit =
        match state.Selection with
        | VocabGroup word_lists ->
            let all_cards = state.FilteredWords(word_lists)
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

        | _ -> ()

    [<Extension>]
    static member DispatchCommand(state: MenuState, command: string) : unit =
        let split =
            command.Split(' ', 2, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

        let command, args = split.[0], if split.Length > 1 then split.[1] else ""

        match command with
        | "exit" -> state.Exit()
        | "echo" -> state.Echo(args)
        | "up" -> state.PreviousSelection()
        | "down" -> state.NextSelection()
        | "stats" -> state.VocabStats()
        | "learn" -> state.Learn()
        | "review" -> state.Review()
        | "ahead" -> state.VocabReviewAhead()
        | "chores" -> state.VocabChoresList()
        | "browse" -> state.Browse()
        | "filter" -> state.CycleFilter()
        | "batch_down" -> state.DecreaseBatchSize()
        | "batch_up" -> state.IncreaseBatchSize()
        | "sync" -> state.Sync()
        | _ -> state.UIContext.StatusLine <- sprintf "Unrecognised command '%s'" command

    [<Extension>]
    static member DispatchMessage(state: MenuState, message: string) : unit =
        if message.StartsWith(':') then
            state.DispatchCommand(message.Substring(1))
        else
            state.UIContext.StatusLine <- sprintf "Unrecognised message '%s'" message

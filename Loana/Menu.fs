namespace Loana

open System
open System.Drawing
open Loana.CLI
open Loana.Data
open Loana.Vocab
open Loana.Quizzes
open Loana.Verbs

type MenuSelection =
    | VocabGroup of string list
    | VerbMode
    | Quiz of Quiz

type Menu(words: WordBank, verbs: VerbBank, scheduler: ReviewSchedule) =

    let vocab: VocabDeck = VocabDeck(scheduler, words)
    let quizzes: QuizScheduler = QuizScheduler(scheduler)
    let verb_deck: VerbDeck = VerbDeck(scheduler, words, verbs)

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
        id, "None";
        (fun cards -> vocab.FilterByTier(cards, 1, 1)), "New words only"
        (fun cards -> vocab.FilterByTier(cards, 2, 999)), "Unlocks only"
        (fun cards ->
            let easier_cards = cards |> Seq.choose(_.Meta.BumpKey) |> Set.ofSeq
            cards |> Seq.filter(fun x -> not (easier_cards.Contains(x.Key)))
        ), "Bump-first"
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

            MenuRender.Write( $" %5i{Seq.length learning} ", Color.LightBlue, Color.FromArgb(0x101020 * m))
            MenuRender.Write( $" %5i{Seq.length due} ", Color.Green, Color.FromArgb(0x102010 * m))
            MenuRender.Write( $" %5i{Seq.length ahead} ", Color.Yellow, Color.FromArgb(0x202010 * m))
            MenuRender.Write( $" %5i{Seq.length available} ", Color.White, Color.FromArgb(0x202020 * m))

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

            MenuRender.Write("[", Color.FromArgb(0x606060 * m), Color.FromArgb(0x303030 * m))
            MenuRender.Write(String.replicate m_c " ", Color.White, Color.Green)
            MenuRender.Write(String.replicate (s_c - m_c) " ", Color.White, Color.FromArgb(0x30D070))
            MenuRender.Write(String.replicate l_c " ", Color.White, Color.FromArgb(0x80AAFF))
            MenuRender.Write("]", Color.FromArgb(0x606060 * m), Color.FromArgb(0x303030 * m))
            MenuRender.Write((sprintf " %.1f%% " mature_percent).PadRight(8), Color.Green, Color.FromArgb(0x102010 * m))
            MenuRender.Write(sprintf "| %5i " total_cards, Color.White, Color.FromArgb(0x202020 * m))

        MenuRender.Write(" Loana Dashboard :) ".PadRight(BAR_SIZE + 3), Color.White, Color.FromArgb(0x202020))
        MenuRender.Write(
            $"  Filter: {snd current_filter} ".PadRight(28),
            (if snd current_filter = "None" then Color.LightGray else Color.DeepPink),
            Color.FromArgb(0x101010)
        )
        MenuRender.Write("".PadLeft(BAR_SIZE - 8), Color.White, Color.FromArgb(0x202020))
        MenuRender.Write( $" %3i{vocab.LearnBatchSize} ", Color.LightBlue, Color.FromArgb(0x202040))
        MenuRender.Write( $" %3i{vocab.ReviewBatchSize} ", Color.Green, Color.FromArgb(0x204020))
        MenuRender.Write( $" %i{Seq.length (vocab.Chores() |> Seq.filter _.Urgent)} chores ".PadLeft(16), Color.Pink, Color.FromArgb(0x202020))
        MenuRender.WriteLine()
        for group in words.Groups do

            let word_lists = List.ofSeq group.Lists
            MenuRender.Write(
                $"@ {group.Name.PadRight(BAR_SIZE).Substring(0, BAR_SIZE)} ",
                (if selection = VocabGroup word_lists then Color.Yellow else Color.White),
                Color.FromArgb(0x303030)
            )
            card_actions(word_lists, true)
            progress_bar(word_lists, true)
            MenuRender.WriteLine()

            for wl in group.Lists do
                MenuRender.Write(
                    $"| {wl.PadRight(BAR_SIZE).Substring(0, BAR_SIZE)} ",
                    (if selection = VocabGroup [wl] then Color.Yellow else Color.LightGreen),
                    Color.FromArgb(0x202020)
                )
                card_actions([wl], false)
                progress_bar([wl], false)
                MenuRender.WriteLine()

        MenuRender.Write(
            "** ALL CARDS ** ".PadRight(BAR_SIZE + 3),
            (if selection = VocabGroup [] then Color.Yellow else Color.White),
            Color.FromArgb(0x303030)
        )
        card_actions([], true)
        progress_bar([], true)
        MenuRender.WriteLine()
        
    member this.RenderVerbModeDashboard() =
        let BAR_SIZE = (MenuRender.Width - 28 - 21) / 2
        MenuRender.Write(
            "** VERB MODE ** ".PadRight(BAR_SIZE + 3),
            (if selection = VerbMode then Color.Yellow else Color.White),
            Color.FromArgb(0x101010)
        )
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        let available = verb_deck.AvailableEntries()
        let learning = verb_deck.LearningEntries(available)
        let due = verb_deck.DueReviewEntries(available, now)
        let ahead = verb_deck.AheadReviewEntries(available, now)

        MenuRender.Write( $" %5i{Seq.length learning} ", Color.LightBlue, Color.FromArgb(0x101020))
        MenuRender.Write( $" %5i{Seq.length due} ", Color.Green, Color.FromArgb(0x102010))
        MenuRender.Write( $" %5i{Seq.length ahead} ", Color.Yellow, Color.FromArgb(0x202010))
        MenuRender.Write( $" %5i{Seq.length available} ", Color.White, Color.FromArgb(0x202020))

        MenuRender.Write(String.replicate (BAR_SIZE + 2) " ", Color.White, Color.FromArgb(0x101010))
        MenuRender.Write(" ----- ".PadRight(8), Color.Green, Color.FromArgb(0x102010))
        MenuRender.Write("| ----- ", Color.White, Color.FromArgb(0x202020))
        MenuRender.WriteLine()

    member this.RenderQuizDashboard() =

        let BAR_SIZE = 45
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        let progress_bar(data: ReviewData voption) =
            match data with
            | ValueNone ->
                MenuRender.Write("[", Color.FromArgb(0x606060), Color.FromArgb(0x303030))
                MenuRender.Write(" N/A ".PadRight(BAR_SIZE), Color.LightGray, Color.FromArgb(0x606060))
                MenuRender.Write("]", Color.FromArgb(0x606060), Color.FromArgb(0x303030))
            | ValueSome data ->
                let progress = if now > data.NextReview then 1.0f else float32 (now - data.LastReviewed) / float32 data.Interval
                let f_c = progress * float32 BAR_SIZE |> floor |> int
                let e_c = BAR_SIZE - f_c
                MenuRender.Write("[", Color.FromArgb(0x606060), Color.FromArgb(0x303030))
                MenuRender.Write(String.replicate f_c " ", Color.White, Color.Green)
                MenuRender.Write(String.replicate e_c " ", Color.White, Color.FromArgb(0x303030))
                MenuRender.Write("]", Color.FromArgb(0x606060), Color.FromArgb(0x303030))

        for quiz in quizzes.Quizzes do
            let schedule = scheduler.Get quiz.Key
            let level = schedule |> ValueOption.map _.Level |> ValueOption.defaultValue 0
            let next_review = schedule |> ValueOption.map _.NextReview |> ValueOption.defaultValue now
            MenuRender.Write($"| {quiz.Name} ".PadRight(MenuRender.Width - 72), (if selection = Quiz quiz then Color.Yellow else Color.LightGreen), Color.FromArgb(0x202020))
            MenuRender.Write($" Level %i{level} ", ReviewData.LevelColors.[level], Color.FromArgb(ReviewData.LevelColors.[level].ToArgb() / 2))
            progress_bar schedule
            if next_review <= now then
                MenuRender.Write(" DUE ".PadLeft(16), Color.Green, Color.FromArgb(0x202020))
            else
                MenuRender.Write($" {MenuRender.FormatInterval(next_review - now)} ".PadLeft(16), Color.Yellow, Color.FromArgb(0x202020))
            MenuRender.WriteLine()

    member this.Run() =
        let mutable loop = true
        while loop do
            MenuRender.UpdateWidth()
            this.RenderVocabDashboard()
            this.RenderVerbModeDashboard()
            this.RenderQuizDashboard()

            match selection with
            | VocabGroup wordlists ->
                MenuRender.WriteLine(MenuRender.Pad " [Enter] Stats  [L] Learn  [R] Review  [A] Review ahead  [C] Chores  [F] Filter ", Color.LightGray, Color.FromArgb(0x303030))
                MenuRender.Redraw()

                match Console.ReadKey(true).Key with
                | ConsoleKey.Escape -> loop <- false
                | ConsoleKey.UpArrow -> previous_selection()
                | ConsoleKey.DownArrow -> next_selection()
                | ConsoleKey.Enter -> vocab.Stats(get_filtered(wordlists))
                | ConsoleKey.L -> vocab.Learn(get_filtered(wordlists))
                | ConsoleKey.R -> vocab.Review(get_filtered(wordlists))
                | ConsoleKey.A -> vocab.ReviewAhead(get_filtered(wordlists))
                | ConsoleKey.C -> vocab.ChoresList()
                | ConsoleKey.F -> cycle_filter()
                | ConsoleKey.OemMinus
                | ConsoleKey.Subtract -> vocab.DecreaseBatchSize()
                | ConsoleKey.OemPlus
                | ConsoleKey.Add -> vocab.IncreaseBatchSize()
                | _ -> ()
                
            | VerbMode ->
                MenuRender.WriteLine(MenuRender.Pad " [L] Learn  [R] Review ", Color.LightGray, Color.FromArgb(0x303030))
                MenuRender.Redraw()

                match Console.ReadKey(true).Key with
                | ConsoleKey.Escape -> loop <- false
                | ConsoleKey.UpArrow -> previous_selection()
                | ConsoleKey.DownArrow -> next_selection()
                | ConsoleKey.L -> verb_deck.Learn(verb_deck.LearningEntries(verb_deck.AvailableEntries()))
                | ConsoleKey.R -> verb_deck.Review(verb_deck.DueReviewEntries(verb_deck.AvailableEntries(), DateTimeOffset.UtcNow.ToUnixTimeSeconds()))
                | _ -> ()
                
            | Quiz quiz ->
                MenuRender.WriteLine(MenuRender.Pad " [Enter] Quiz  [A] Auto ", Color.LightGray, Color.FromArgb(0x303030))
                MenuRender.Redraw()

                match Console.ReadKey(true).Key with
                | ConsoleKey.Escape -> loop <- false
                | ConsoleKey.UpArrow -> previous_selection()
                | ConsoleKey.DownArrow -> next_selection()
                | ConsoleKey.Enter -> quizzes.Study(quiz)
                | ConsoleKey.A -> quizzes.Study(quizzes.Auto())
                | _ -> ()
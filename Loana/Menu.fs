namespace Loana

open System
open System.Drawing
open Loana.CLI
open Loana.Data
open Loana.Vocab

type Menu(words: WordBank, scheduler: ReviewSchedule) =

    let vocab: VocabDeck = VocabDeck(scheduler, words)

    let WORD_GROUPS =
        seq {
            for group in words.Groups do
                yield List.ofSeq group.Lists
                yield! group.Lists |> Seq.map List.singleton
            yield []
        }
        |> Array.ofSeq

    let FILTERS = [|
        id, "None";
        (fun cards -> vocab.FilterByTier(cards, 1, 1)), "New words only"
        (fun cards -> vocab.FilterByTier(cards, 2, 999)), "Unlocks only"
    |]

    let mutable selected_group = []
    let mutable current_filter = FILTERS.[0]

    let next_selection() =
        selected_group <- WORD_GROUPS.[(Array.IndexOf(WORD_GROUPS, selected_group) + 1) % WORD_GROUPS.Length]
    let previous_selection() =
        selected_group <- WORD_GROUPS.[(Array.IndexOf(WORD_GROUPS, selected_group) + WORD_GROUPS.Length - 1) % WORD_GROUPS.Length]

    let cycle_filter() =
        current_filter <- FILTERS.[(Array.IndexOf(FILTERS, current_filter) + 1) % FILTERS.Length]

    let get_filtered() =
        vocab.AvailableCards(selected_group) |> fst current_filter

    member private this.RenderVocabDashboard() =
        let BAR_SIZE = (MenuRender.Width - 28 - 21) / 2

        let card_actions(word_lists: string list, is_group: bool) =
            let available = vocab.AvailableCards(word_lists) |> fst current_filter
            let learning = vocab.LearningCards(available)
            let due = vocab.DueReviewCards(available, DateTimeOffset.UtcNow.ToUnixTimeSeconds())
            let ahead = vocab.AheadReviewCards(available, DateTimeOffset.UtcNow.ToUnixTimeSeconds())

            let m = if is_group then 2 else 1

            MenuRender.Write(sprintf " % 5i " (Seq.length learning), Color.LightBlue, Color.FromArgb(0x101020 * m))
            MenuRender.Write(sprintf " % 5i " (Seq.length due), Color.Green, Color.FromArgb(0x102010 * m))
            MenuRender.Write(sprintf " % 5i " (Seq.length ahead), Color.Yellow, Color.FromArgb(0x202010 * m))
            MenuRender.Write(sprintf " % 5i " (Seq.length available), Color.White, Color.FromArgb(0x202020 * m))

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
            MenuRender.Write(sprintf "| % 5i " total_cards, Color.White, Color.FromArgb(0x202020 * m))

        MenuRender.Write(" The Word Eater :) ".PadRight(BAR_SIZE + 3), Color.White, Color.FromArgb(0x202020))
        MenuRender.Write(
            $"  Filter: {snd current_filter} ".PadRight(28),
            (if snd current_filter = "None" then Color.LightGray else Color.DeepPink),
            Color.FromArgb(0x101010)
        )
        MenuRender.Write((sprintf " %i chores " (Seq.length (vocab.Chores() |> Seq.filter _.Urgent))).PadLeft(BAR_SIZE + 18), Color.Pink, Color.FromArgb(0x202020))
        MenuRender.WriteLine()
        for group in words.Groups do

            let word_lists = List.ofSeq group.Lists
            MenuRender.Write(
                $"@ {group.Name.PadRight(BAR_SIZE).Substring(0, BAR_SIZE)} ",
                (if selected_group = word_lists then Color.Yellow else Color.White),
                Color.FromArgb(0x303030)
            )
            card_actions(word_lists, true)
            progress_bar(word_lists, true)
            MenuRender.WriteLine()

            for wl in group.Lists do
                MenuRender.Write(
                    $"| {wl.PadRight(BAR_SIZE).Substring(0, BAR_SIZE)} ",
                    (if selected_group = [wl] then Color.Yellow else Color.LightGreen),
                    Color.FromArgb(0x202020)
                )
                card_actions([wl], false)
                progress_bar([wl], false)
                MenuRender.WriteLine()

        MenuRender.Write(
            "** ALL CARDS ** ".PadRight(BAR_SIZE + 3),
            (if selected_group = [] then Color.Yellow else Color.White),
            Color.FromArgb(0x303030)
        )
        card_actions([], true)
        progress_bar([], true)
        MenuRender.WriteLine()
        MenuRender.WriteLine(MenuRender.Pad " [Enter] Stats  [L] Learn  [R] Review  [A] Review ahead  [C] Chores  [F] Filter ", Color.LightGray, Color.FromArgb(0x303030))
        MenuRender.Redraw()

    member this.Run() =
        let mutable loop = true
        while loop do

            MenuRender.UpdateWidth()
            this.RenderVocabDashboard()

            match Console.ReadKey(true).Key with
            | ConsoleKey.UpArrow -> previous_selection()
            | ConsoleKey.DownArrow -> next_selection()
            | ConsoleKey.Escape -> loop <- false
            | ConsoleKey.Enter -> vocab.Stats(get_filtered())
            | ConsoleKey.L -> vocab.Learn(get_filtered())
            | ConsoleKey.R -> vocab.Review(get_filtered())
            | ConsoleKey.A -> vocab.ReviewAhead(get_filtered())
            | ConsoleKey.C -> vocab.ChoresList()
            | ConsoleKey.F -> cycle_filter()
            | ConsoleKey.Q -> Quizzes.QuizScheduler(scheduler).Study()
            | _ -> ()
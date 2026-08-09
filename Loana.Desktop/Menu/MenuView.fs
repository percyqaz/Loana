namespace Loana.Desktop

open System
open System.Drawing
open Loana.Data
open Loana.Vocab
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

    member this.Run() : unit =
        let buffer = CommandBuffer()
        buffer.Bind("<Esc>", ":exit<Enter>")
        buffer.Bind("j", ":down<Enter>")
        buffer.Bind("<Down>", "j")
        buffer.Bind("k", ":up<Enter>")
        buffer.Bind("<Up>", "k")
        buffer.Bind("<Enter>", ":stats<Enter>")
        buffer.Bind("r", ":review<Enter>")
        buffer.Bind("l", ":learn<Enter>")
        buffer.Bind("a", ":ahead<Enter>")
        buffer.Bind("c", ":chores<Enter>")
        buffer.Bind("f", ":filter<Enter>")
        buffer.Bind("-", ":batch_down<Enter>")
        buffer.Bind("=", ":batch_up<Enter>")
        buffer.Bind("s", ":sync<Enter>")

        while state.Running do
            MenuRender.UpdateWidth()
            this.RenderVocabDashboard()
            this.RenderVerbModeDashboard()
            this.RenderQuizDashboard()

            let guide =
                match state.Selection with
                | VocabGroup _ -> " [Enter] Stats  [L] Learn  [R] Review  [A] Review ahead  [C] Chores  [F] Filter "
                | VerbMode -> " [L] Learn  [R] Review "
                | Quiz _ -> " [R] Quiz "

            MenuRender.WriteLine(MenuRender.Pad(guide), Color.LightGray, Color.FromArgb(0xFF_303030))
            MenuRender.Redraw()
            
            Console.Write(buffer.ToString().ForeColor(Color.LightGreen).Bold().ClearRestOfLine())
            
            buffer.AddKey(Console.ReadKey(true))
            buffer.Dispatch(state.DispatchMessage)
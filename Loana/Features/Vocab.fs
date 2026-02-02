namespace Loana.Features

open System
open System.Drawing
open System.Runtime.CompilerServices
open Loana.CLI
open Loana.Language
open Loana.Scheduler
open Loana.GUI

[<Extension>]
type CardExtensions =

    [<Extension>]
    static member RecallEnToDe(v: Vocab) =
        let annotation_html(a: Annotation) =
            match a.Note with
            | Some n -> $"""{a.Text} <span class="note">[{n}]</span>"""
            | None -> a.Text
        let en_html = (v.English :: v.EnglishAlternatives) |> Seq.map annotation_html |> String.concat ", "
        {
            Key = $"vocab-recall-{v.Key}"
            Front =
                $"""
                <div class="en-de">
                <div class="en">{en_html}</div>
                <div class="de">???</div>
                </div>
                """
            Back =
                $"""
                <div class="en-de">
                <div class="en">{en_html}</div>
                <div class="de">{v.Deutsch}</div>
                </div>
                """
        }

    [<Extension>]
    static member RecogniseDeToEn(v: Vocab) =
        let annotation_html(a: Annotation) =
            match a.Note with
            | Some n -> $"""{a.Text} <span class="note">[{n}]</span>"""
            | None -> a.Text
        let en_html = (v.English :: v.EnglishAlternatives) |> Seq.map annotation_html |> String.concat ", "
        {
            Key = $"vocab-recognise-{v.Key}"
            Front =
                $"""
                <div class="de-en">
                <div class="de">{v.Deutsch}</div>
                <div class="en">???</div>
                </div>
                """
            Back =
                $"""
                <div class="de-en">
                <div class="de">{v.Deutsch}</div>
                <div class="en">{en_html}</div>
                </div>
                """
        }

    [<Extension>]
    static member ArticleRecallEnToDe(n: Noun) =
        let annotation_html(a: Annotation) =
            match a.Note with
            | Some n -> $"""<span class="note">the </span>{a.Text} <span class="note">[{n}]</span>"""
            | None -> $"""<span class="note">the </span>{a.Text}"""
        let de_html =
            let article = AnnotationTree.flatten_tree (Deutsch.definite_article n.Guts.Gender Case.Nominative)
            $"""<span class="note">{article} </span><span style="color:#{n.Guts.Gender.Color.ToArgb().ToString("X06")};">{n.Deutsch}</span>"""
        let en_html = (n.English :: n.EnglishAlternatives) |> Seq.map annotation_html |> String.concat ", "
        {
            Key = $"noun-recall-{n.KeyWithGender}"
            Front =
                $"""
                <div class="en-de">
                <div class="en">{en_html}</div>
                <div class="de">???</div>
                </div>
                """
            Back =
                $"""
                <div class="en-de">
                <div class="en">{en_html}</div>
                <div class="de">{de_html}</div>
                </div>
                """
        }

    [<Extension>]
    static member ArticleRecogniseDeToEn(n: Noun) =
        let annotation_html(a: Annotation) =
            match a.Note with
            | Some n -> $"""<span class="note">the </span>{a.Text} <span class="note">[{n}]</span>"""
            | None -> $"""<span class="note">the </span>{a.Text}"""
        let de_html =
            let article = AnnotationTree.flatten_tree (Deutsch.definite_article n.Guts.Gender Case.Nominative)
            $"""<span class="note">{article} </span><span style="color:#{n.Guts.Gender.Color.ToArgb().ToString("X06")};">{n.Deutsch}</span>"""
        let en_html = (n.English :: n.EnglishAlternatives) |> Seq.map annotation_html |> String.concat ", "
        {
            Key = $"noun-recognise-{n.KeyWithGender}"
            Front =
                $"""
                <div class="de-en">
                <div class="de">{de_html}</div>
                <div class="en">???</div>
                </div>
                """
            Back =
                $"""
                <div class="de-en">
                <div class="de">{de_html}</div>
                <div class="en">{en_html}</div>
                </div>
                """
        }

type VocabDeck(scheduler: ReviewSchedule, wordlist: Wordlist) =

    let level_of (c: GuiCard) : int =
        match scheduler.Get c.Key with
        | ValueSome data -> data.Level
        | ValueNone -> 0

    let vocab_cards(v: Vocab) : GuiCard seq =
        seq {
            let tier_1 = v.RecogniseDeToEn()
            yield tier_1

            if level_of tier_1 >= 2 then
                yield v.RecallEnToDe()
        }

    let noun_cards(n: Noun) : GuiCard seq =
        seq {
            let tier_1 = n.Translation.RecogniseDeToEn()
            let tier_2 = n.Translation.RecallEnToDe()
            let tier_3 = n.ArticleRecogniseDeToEn()

            if level_of tier_1 < 2 then
                yield tier_1
            elif level_of tier_2 < 4 then
                yield tier_1
                yield tier_2
            elif level_of tier_3 < 2 then
                yield tier_2
                yield tier_3
            else
                yield tier_3
                let tier_4 = n.ArticleRecallEnToDe()
                yield tier_4
        }

    member this.Chores() =
        seq {
            for word in wordlist.Entries do
                match word.Item with
                | Vocab v when v.DetectNoun ->
                    match v.RecallEnToDe().Key |> scheduler.Get with
                    | ValueSome d when d.Level >= 4 ->
                        yield sprintf "'%O' in '%s' is missing gender!" v.Deutsch word.Source
                    | _ -> ()
                | _ -> ()
                // nouns without plural
        }

    member this.AvailableCards(sources: string list) =
        seq {
            for word in wordlist.Entries do
                if sources.IsEmpty || List.contains word.Source sources then
                    match word.Item with
                    | Vocab v -> yield! vocab_cards v
                    | Noun n -> yield! noun_cards n
                    | Verb v -> yield! vocab_cards v.Infinitive
        }
        |> Seq.cache

    member this.AvailableCards() = this.AvailableCards([])

    member this.FilterByLevel(cards: GuiCard seq, minlevel: int, maxlevel: int) =
        cards
        |> Seq.where (fun c ->
            match scheduler.Get c.Key with
            | ValueSome data -> data.Level >= minlevel && data.Level <= maxlevel
            | ValueNone -> false
        )

    member this.LearningCards(cards: GuiCard seq) =
        cards
        |> Seq.where (fun c -> (scheduler.Get c.Key).IsNone)

    member this.ReviewCards(cards: GuiCard seq) =
        cards
        |> Seq.where (fun c -> (scheduler.Get c.Key).IsSome)

    member this.DueReviewCards(cards: GuiCard seq, now: int64) =
        cards
        |> Seq.choose (fun c ->
            match scheduler.Get c.Key with
            | ValueSome data ->
                let dl = data.DueLevel now
                if dl >= 0 then Some (c, dl) else None
            | ValueNone -> None
        )
        |> Seq.sortByDescending snd
        |> Seq.map fst

    member this.AheadReviewCards(cards: GuiCard seq, now: int64) =
        cards
        |> Seq.choose (fun c ->
            match scheduler.Get c.Key with
            | ValueSome data ->
                let n = data.NextReview
                if n > now then Some (c, n) else None
            | ValueNone -> None
        )
        |> Seq.sortBy snd
        |> Seq.map fst

    member this.Stats(cards: GuiCard seq) : (int * int) seq =
        cards
        |> Seq.choose (fun card -> scheduler.Get card.Key |> ValueOption.map _.Level |> ValueOption.toOption)
        |> Seq.countBy id
        |> Seq.sortBy fst

    member this.Study() =

        App.StartThread()

        let review () =
            let cards = this.DueReviewCards(this.AvailableCards(), DateTimeOffset.UtcNow.ToUnixTimeSeconds()) |> Seq.truncate 50 |> Array.ofSeq
            HtmlWindow.ShowUntilClosed(ReviewSession(cards, scheduler).Init)

        let review_ahead () =
            let cards = this.AheadReviewCards(this.AvailableCards(), DateTimeOffset.UtcNow.ToUnixTimeSeconds()) |> Seq.truncate 50 |> Array.ofSeq
            HtmlWindow.ShowUntilClosed(ReviewSession(cards, scheduler).Init)

        let learn () =
            let cards = this.LearningCards(this.AvailableCards()) |> Seq.truncate 20 |> Array.ofSeq
            HtmlWindow.ShowUntilClosed(LearnSession(cards, scheduler).Init)

        let chores () =
            Console.WriteLine(" Chores ", Color.White, Color.FromArgb(0x202020))
            for chore in this.Chores() |> Seq.truncate 20 do
                Console.WriteLine(chore, Color.Pink)
            Console.ReadLine() |> ignore

        let stats () =
            let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
            let all_cards = this.AvailableCards()
            Console.WriteLine(" All cards ", Color.White, Color.FromArgb(0x202020))
            all_cards
            |> this.Stats
            |> Seq.iter (fun (level, count) ->
                Console.WriteLine(sprintf "[%i] %i cards" level count, Color.LightGray)
            )
            Console.WriteLine(" Next 100 cards ", Color.White, Color.FromArgb(0x202020))
            this.AheadReviewCards(all_cards, now)
            |> Seq.truncate 100
            |> this.Stats
            |> Seq.iter (fun (level, count) ->
                Console.WriteLine(sprintf "[%i] %i cards" level count, Color.LightGray)
            )
            Console.WriteLine(" Upcoming workload ", Color.White, Color.FromArgb(0x202020))

            let upcoming(label: string, days: int64) =
                this.AheadReviewCards(all_cards, now)
                |> Seq.takeWhile (fun c -> (scheduler.Get c.Key).Value.NextReview < now + TimeSpan.SecondsPerDay * days)
                |> Seq.length
                |> fun x -> Console.WriteLine(sprintf "%s: %i cards" label x, Color.LightGray)

            upcoming("1d", 1L)
            upcoming("2d", 2L)
            upcoming("1w", 7L)
            upcoming("2w", 14L)

            for wl in wordlist.Sources do
                Console.Write($" {wl} ", Color.LightGreen, Color.FromArgb(0x202020))
                let available = this.AvailableCards([wl])
                Console.Write(sprintf " %i available " (Seq.length available), Color.White, Color.FromArgb(0x202020))
                let learning = this.LearningCards(available)
                Console.WriteLine(sprintf " %i to learn" (Seq.length learning), Color.LightBlue, Color.FromArgb(0x202020))

            Console.ReadLine() |> ignore

        // todo: study by level, study by wordlist

        let mutable loop = true
        while loop do
            Console.Clear()
            Console.WriteLine("Vocab learner :)")
            let available = this.AvailableCards()
            Console.WriteLine(sprintf " %i cards available " (Seq.length available), Color.White, Color.FromArgb(0x202020))
            let learning = this.LearningCards(available)
            Console.WriteLine(sprintf " %i cards to [learn] " (Seq.length learning), Color.LightBlue, Color.FromArgb(0x202020))
            let due = this.DueReviewCards(available, DateTimeOffset.UtcNow.ToUnixTimeSeconds())
            Console.WriteLine(sprintf " %i cards to [review] " (Seq.length due), Color.Green, Color.FromArgb(0x202020))
            let ahead = this.AheadReviewCards(available, DateTimeOffset.UtcNow.ToUnixTimeSeconds())
            Console.WriteLine(sprintf " %i cards [ahead] " (Seq.length ahead), Color.Yellow, Color.FromArgb(0x202020))
            Console.WriteLine(sprintf " %i [chores] " (Seq.length (this.Chores())), Color.Red, Color.FromArgb(0x202020))

            match Console.ReadLine() with
            | "review" -> review()
            | "ahead" -> review_ahead()
            | "learn" -> learn()
            | "chores" -> chores()
            | "stats" -> stats()
            | "back" -> loop <- false
            | _ -> ()
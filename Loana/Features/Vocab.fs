namespace Loana.Decks

open System
open System.Drawing
open Loana.CLI
open Loana.Language
open Loana.Scheduler
open Loana.GUI

type VocabDeck(scheduler: ReviewSchedule, wordlist: Wordlist) =

    let de_to_en(v: Vocab) : GuiCard =
        let annotation_html(a: Annotation) =
            match a.Note with
            | Some n -> $"""{a.Text} <span class="note">[{n}]</span>"""
            | None -> a.Text
        let en_html = (v.English :: v.EnglishAlternatives) |> Seq.map annotation_html |> String.concat ", "
        {
            Key = $"vocab-recognise-{v.Key}"
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

    let en_to_de(v: Vocab) : GuiCard =
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

    let vocab_cards(v: Vocab) : GuiCard seq =
        seq {
            let tier_1 = de_to_en v
            yield tier_1

            match scheduler.Get tier_1.Key with
            | ValueSome d when d.Level >= 3 ->
                let tier_2 = en_to_de v
                yield tier_2
            | _ -> ()
        }

    member this.AllAvailableCards() =
        seq {
            for word in wordlist.Entries do
                match word.Item with
                | Vocab v -> yield! vocab_cards v
                | Noun n -> yield! vocab_cards n.Translation // todo: plural forms, gender
                | Verb v -> yield! vocab_cards v.Infinitive
        }
        |> Seq.cache

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

    member this.AheadReviewCards(cards: GuiCard seq) =
        cards
        |> Seq.choose (fun c ->
            match scheduler.Get c.Key with
            | ValueSome data -> Some (c, data.NextReview)
            | ValueNone -> None
        )
        |> Seq.sortBy snd
        |> Seq.map fst

    member this.Study() =

        let review () =
            let cards = this.DueReviewCards(this.AllAvailableCards(), DateTimeOffset.UtcNow.ToUnixTimeSeconds()) |> Seq.truncate 50 |> Array.ofSeq
            App.Run(fun () -> ReviewSession(cards, scheduler).Init(HtmlWindow())) |> ignore

        let review_ahead () =
            let cards = this.AheadReviewCards(this.AllAvailableCards()) |> Seq.truncate 50 |> Array.ofSeq
            App.Run(fun () -> ReviewSession(cards, scheduler).Init(HtmlWindow())) |> ignore

        let learn () =
            let cards = this.LearningCards(this.AllAvailableCards()) |> Seq.truncate 20 |> Array.ofSeq
            App.Run(fun () -> LearnSession(cards, scheduler).Init(HtmlWindow())) |> ignore

        let mutable loop = true
        while loop do
            Console.WriteLine("Vocab learner :)")
            let available = this.AllAvailableCards()
            Console.WriteLine(sprintf " %i cards available " (Seq.length available), Color.White, Color.FromArgb(0x202020))
            let learning = this.LearningCards(available)
            Console.WriteLine(sprintf " %i cards to learn " (Seq.length learning), Color.LightBlue, Color.FromArgb(0x202020))
            let due = this.DueReviewCards(available, DateTimeOffset.UtcNow.ToUnixTimeSeconds())
            Console.WriteLine(sprintf " %i cards due " (Seq.length due), Color.Green, Color.FromArgb(0x202020))
            let ahead = this.AheadReviewCards(available)
            Console.WriteLine(sprintf " %i cards ok " (Seq.length ahead), Color.Yellow, Color.FromArgb(0x202020))

            match Console.ReadLine() with
            | "review" -> review()
            | "ahead" -> review_ahead()
            | "learn" -> learn()
            | "back" -> loop <- false
            | _ -> ()
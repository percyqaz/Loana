namespace Loana.Scheduler

open System
open Loana

[<AbstractClass>]
type Card(key: string, spacing_rule: CardSpacingRule, scheduler: CardScheduler) =

    member this.Schedule : CardScheduleData = scheduler.Get(key)
    member this.Reschedule(result: CardEase, now: int64) : unit =
            scheduler.Review(key, spacing_rule, result, now)
    member this.IsDue(now: int64) = this.Schedule.NextReview <= now

    abstract member DisplayFront: IOutput -> unit
    abstract member DisplayBack: IOutput -> unit
    abstract member FrontInput: string * IOutput -> CardEase option // None = show back
    abstract member BackInput: string * IOutput -> CardEase

type CardStack =
    private {
        Source: Card seq
        Stack: ResizeArray<Card>
        TimeStart: int64
        TimeOffset: int64
        AllowReplacement: bool
    }

    static member GetDueCards (source: Card seq, now: int64) : Card seq =
        source
        |> Seq.filter (fun card -> card.Schedule.NextReview <= now)
        |> Seq.sortBy (fun card -> card.Schedule.NextReview)

    static member Build(source: Card seq, allow_replacement: bool, limit: int, offset: int64) : CardStack =
        let start = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        let now = start + offset

        let due_cards =
            CardStack.GetDueCards(source, now)
            |> Seq.truncate limit
            |> Array.ofSeq

        Random().Shuffle(due_cards)
        {
            Source = source
            Stack = ResizeArray<Card>(due_cards)
            TimeStart = start
            TimeOffset = offset
            AllowReplacement = allow_replacement
        }

    static member Build(source: Card seq, allow_replacement: bool, limit: int) : CardStack =
        CardStack.Build(source, allow_replacement, limit, 0L)

    static let SECONDS_PER_CARD = 6L
    static let THIRTY_SECONDS_IN_CARDS = 5

    member this.Remaining = this.Stack.Count

    member this.ReplaceCard (card: Card, result: CardEase) : unit =
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds() + this.TimeOffset
        card.Reschedule(result, now)
        if this.AllowReplacement then

            let schedule = card.Schedule
            let how_far_in_future = schedule.NextReview - now
            let cards_in_future = how_far_in_future / SECONDS_PER_CARD |> int

            if schedule.LearningStep.IsSome || cards_in_future <= THIRTY_SECONDS_IN_CARDS || cards_in_future < this.Stack.Count then
                this.Stack.Insert(min this.Stack.Count (max cards_in_future THIRTY_SECONDS_IN_CARDS), card)

    member this.GetNextCard () : Card option =
        if this.Stack.Count > 0 then
            let card = this.Stack.[0]
            this.Stack.RemoveAt(0)
            Some card
        else
            None
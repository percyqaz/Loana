namespace Loana.Scheduler

open System
open Loana

[<RequireQualifiedAccess>]
type CardResult =
    | Forgot
    | Bad
    | Okay
    | Easy

[<Interface>]
type ICard =
    abstract member ScheduledTime: int64 with get
    abstract member DisplayFront: IOutput -> unit
    abstract member DisplayBack: IOutput -> unit
    abstract member FrontInput: string * IOutput -> CardResult option // None = show back
    abstract member BackInput: string * IOutput -> CardResult
    abstract member Reschedule: CardResult * int64 -> unit

type CardSchedule =
    private {
        Source: ICard seq
        Stack: ResizeArray<ICard>
        TimeStart: int64
        TimeOffset: int64
        AllowReplacement: bool
    }

    static member GetDueCards (source: ICard seq, now: int64) : ICard seq =
        source
        |> Seq.filter (fun card -> card.ScheduledTime <= now)
        |> Seq.sortBy (fun card -> card.ScheduledTime)

    static member Build(source: ICard seq, allow_replacement: bool, limit: int, offset: int64) : CardSchedule =
        let start = DateTime.UtcNow.Ticks
        let now = start + offset

        let due_cards =
            CardSchedule.GetDueCards(source, now)
            |> Seq.truncate limit
            |> Array.ofSeq

        Random().Shuffle(due_cards)
        {
            Source = source
            Stack = ResizeArray<ICard>(due_cards)
            TimeStart = start
            TimeOffset = offset
            AllowReplacement = allow_replacement
        }

    static member Build(source: ICard seq, allow_replacement: bool, limit: int) : CardSchedule =
        CardSchedule.Build(source, allow_replacement, limit, 0L)

    static let SIX_SECONDS_PER_CARD = TimeSpan.TicksPerSecond * 6L
    static let THIRTY_SECONDS_IN_CARDS = 5

    member this.Remaining = this.Stack.Count

    member this.ReplaceCard (card: ICard, result: CardResult) : unit =
        let now = DateTime.UtcNow.Ticks + this.TimeOffset
        card.Reschedule(result, now)
        if this.AllowReplacement then
            let how_far_in_future = card.ScheduledTime - now
            let cards_in_future = how_far_in_future / SIX_SECONDS_PER_CARD |> int
            if cards_in_future <= THIRTY_SECONDS_IN_CARDS || cards_in_future < this.Stack.Count then
                this.Stack.Insert(min this.Stack.Count (max cards_in_future THIRTY_SECONDS_IN_CARDS), card)

    member this.GetNextCard () : ICard option =
        if this.Stack.Count > 0 then
            let card = this.Stack.[0]
            this.Stack.RemoveAt(0)
            Some card
        else
            None
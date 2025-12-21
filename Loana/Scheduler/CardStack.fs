namespace Loana.Scheduler

open System
open Loana

[<RequireQualifiedAccess>]
type CardEase =
    | Forgot
    | Bad
    | Okay
    | Easy

[<Interface>]
type ICard =
    abstract member ScheduledTime: int64 with get
    abstract member DisplayFront: IOutput -> unit
    abstract member DisplayBack: IOutput -> unit
    abstract member FrontInput: string * IOutput -> CardEase option // None = show back
    abstract member BackInput: string * IOutput -> CardEase
    abstract member Reschedule: CardEase * int64 -> unit

type CardStack =
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

    static member Build(source: ICard seq, allow_replacement: bool, limit: int, offset: int64) : CardStack =
        let start = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        let now = start + offset

        let due_cards =
            CardStack.GetDueCards(source, now)
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

    static member Build(source: ICard seq, allow_replacement: bool, limit: int) : CardStack =
        CardStack.Build(source, allow_replacement, limit, 0L)

    static let SECONDS_PER_CARD = 6L
    static let THIRTY_SECONDS_IN_CARDS = 5

    member this.Remaining = this.Stack.Count

    member this.ReplaceCard (card: ICard, result: CardEase) : unit =
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds() + this.TimeOffset
        card.Reschedule(result, now)
        if this.AllowReplacement then
            let how_far_in_future = card.ScheduledTime - now
            let cards_in_future = how_far_in_future / SECONDS_PER_CARD |> int
            if cards_in_future <= THIRTY_SECONDS_IN_CARDS || cards_in_future < this.Stack.Count then
                this.Stack.Insert(min this.Stack.Count (max cards_in_future THIRTY_SECONDS_IN_CARDS), card)

    member this.GetNextCard () : ICard option =
        if this.Stack.Count > 0 then
            let card = this.Stack.[0]
            this.Stack.RemoveAt(0)
            Some card
        else
            None
namespace Loana.Scheduler

open System
open Avalonia.Media
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
    abstract member FrontInput: string -> CardResult option // None = show back
    abstract member BackInput: string -> CardResult
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

type QuizState =
    | Start
    | ShowingFront of ICard
    | ShowingBack of ICard
    | Complete

type QuizContext =
    private {
        Log: IOutput
        Display: IOutput
        Deck: CardSchedule
        mutable State: QuizState
    }

    static member Create(deck: CardSchedule, log: IOutput, display: IOutput) : QuizContext =
        {
            Log = log
            Display = display
            Deck = deck
            State = Start
        }

    member private this.NextCard() : bool =
        match this.Deck.GetNextCard() with
        | Some x ->
            this.State <- ShowingFront x
            x.DisplayFront(this.Display)
            true
        | None ->
            this.State <- Complete
            false

    member this.Next(user_input: string) : bool =
        match this.State with
        | Start ->
            this.Log.WriteLine(sprintf "Beginning quiz consisting of %i cards" this.Deck.Remaining, Brushes.LightGreen)
            this.NextCard()

        | ShowingFront card ->
            match card.FrontInput user_input with
            | Some result ->
                this.Deck.ReplaceCard(card, result)
                this.NextCard()
            | None ->
                this.State <- ShowingBack card
                card.DisplayBack(this.Display)
                true

        | ShowingBack card ->
            this.Deck.ReplaceCard(card, user_input |> card.BackInput)
            this.NextCard()

        | Complete -> false
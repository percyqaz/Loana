namespace Loana.GUI

open System
open Loana.GUI
open Loana.Scheduler

type GuiCard =
    {
        Key: string
        Front: string
        Back: string
    }

type private GuiReviewSessionState =
    | Start
    | ShowingFront of GuiCard
    | ShowingBack of GuiCard
    | Complete

[<AbstractClass>]
type GuiReviewSession(cards: GuiCard array) =

    let mutable state = Start

    do Random().Shuffle(cards)

    let cards = ResizeArray<GuiCard>(cards)
    let template = HtmlWindow.GetResource("index.html")

    member this.Init(window: HtmlWindow) : HtmlWindow =

        window.SetCSS (HtmlWindow.GetResource("style.css"))

        let next_card() =
            if cards.Count > 0 then
                let next = cards.[0]
                cards.RemoveAt(0)
                state <- ShowingFront next
                window.Title <- sprintf "Loana Study Session | %i left" cards.Count
                window.SetHtml(template.Replace("{{content}}", next.Front))
            else
                state <- Complete
                window.Close()

        window.KeyDown.Add(fun k ->
            match state with
            | Start -> next_card()
            | ShowingFront current ->
                match k.Key with
                | Avalonia.Input.Key.Space ->
                    state <- ShowingBack current
                    window.SetHtml(template.Replace("{{content}}", current.Back))
                | _ -> ()
            | ShowingBack current ->
                match k.Key with
                | Avalonia.Input.Key.Z ->
                    this.Forget current
                    next_card()
                | Avalonia.Input.Key.OemComma ->
                    this.Demote current
                    next_card()
                | Avalonia.Input.Key.OemPeriod ->
                    this.Keep current
                    next_card()
                | Avalonia.Input.Key.OemQuestion ->
                    this.Promote current
                    next_card()
                | _ -> ()
            | Complete -> ()
        )

        next_card()
        window

    member this.ReplaceNear(card: GuiCard) =
        cards.Insert(min 5 cards.Count, card)

    member this.ReplaceFar(card: GuiCard) =
        cards.Add(card)

    abstract member Forget: GuiCard -> unit
    abstract member Demote: GuiCard -> unit
    abstract member Keep: GuiCard -> unit
    abstract member Promote: GuiCard -> unit

type LearnSession(cards, scheduler: ReviewSchedule) =
    inherit GuiReviewSession(cards)

    override this.Forget (card: GuiCard) = this.ReplaceNear(card)
    override this.Demote (card: GuiCard) = this.ReplaceNear(card)
    override this.Keep (card: GuiCard) = this.ReplaceFar(card)
    override this.Promote (card: GuiCard) = scheduler.Schedule(card.Key, ReviewData.Level1(DateTimeOffset.UtcNow.ToUnixTimeSeconds(), 1))

type ReviewSession(cards, scheduler: ReviewSchedule) =
    inherit GuiReviewSession(cards)

    override this.Forget (card: GuiCard) = scheduler.Reschedule(card.Key, _.Forget)
    override this.Demote (card: GuiCard) = scheduler.Reschedule(card.Key, _.Demote)
    override this.Keep (card: GuiCard) = scheduler.Reschedule(card.Key, _.Keep)
    override this.Promote (card: GuiCard) = scheduler.Reschedule(card.Key, _.Promote)
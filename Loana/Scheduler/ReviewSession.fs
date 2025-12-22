namespace Loana.Scheduler

open Avalonia.Media
open Loana

type private ReviewSessionState =
    | Start
    | ShowingFront of Card
    | ShowingBack of Card
    | Complete

type ReviewSession =
    private {
        Log: IOutput
        Display: IOutput
        Deck: CardStack
        mutable State: ReviewSessionState
    }

    static member Create(deck: CardStack, log: IOutput, display: IOutput) : ReviewSession =
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
            this.Display.Clear()
            this.Display.WriteLine($" {this.Deck.Remaining + 1} remaining ", Brushes.LimeGreen, Brushes.DarkGreen)
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
            match card.FrontInput(user_input, this.Display) with
            | Some result ->
                this.Deck.ReplaceCard(card, result)
                this.NextCard()
            | None ->
                this.State <- ShowingBack card
                card.DisplayBack(this.Display)
                true

        | ShowingBack card ->
            this.Deck.ReplaceCard(card, card.BackInput(user_input, this.Display))
            this.NextCard()

        | Complete -> false
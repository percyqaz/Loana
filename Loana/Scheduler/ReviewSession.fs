namespace Loana.Scheduler

open Avalonia.Media
open Loana
open Loana.Interface

type private ReviewSessionState =
    | Start
    | ShowingFront of Card
    | ShowingBack of Card
    | Complete

type ReviewSession(deck: CardStack, log: IOutput, display: IOutput) =
    inherit Menu(display)

    let mutable state = Start

    member private this.NextCard() : bool =
        match deck.GetNextCard() with
        | Some x ->
            state <- ShowingFront x
            display.Clear()
            display.WriteLine($" {deck.Remaining + 1} remaining ", Brushes.LimeGreen, Brushes.DarkGreen)
            x.DisplayFront(display)
            true
        | None ->
            state <- Complete
            false

    override this.Start() : bool = state.IsStart && this.Next("")

    override this.Next(user_input: string) : bool =
        match state with
        | Start ->
            log.WriteLine(sprintf "Beginning quiz consisting of %i cards" deck.Remaining, Brushes.LightGreen)
            this.NextCard()

        | ShowingFront card ->
            match card.FrontInput(user_input, display) with
            | Some result ->
                deck.ReplaceCard(card, result)
                this.NextCard()
            | None ->
                state <- ShowingBack card
                card.DisplayBack(display)
                true

        | ShowingBack card ->
            deck.ReplaceCard(card, card.BackInput(user_input, display))
            this.NextCard()

        | Complete -> false
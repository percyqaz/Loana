namespace Loana.GUI

open System
open Loana.CLI
open Loana.Study

type LearnSession(cards, scheduler: ReviewSchedule) =
    inherit StudySession(cards)

    override this.Forget (card: Card) = this.ReplaceNear(card)
    override this.Demote (card: Card) = this.ReplaceNear(card)
    override this.Keep (card: Card) = this.ReplaceFar(card)
    override this.Promote (card: Card) = scheduler.Schedule(card.Key, ReviewData.Level1(DateTimeOffset.UtcNow.ToUnixTimeSeconds(), 1)) |> this.Log

type ReviewSession(cards, scheduler: ReviewSchedule) =
    inherit StudySession(cards)

    override this.Forget (card: Card) = scheduler.Reschedule(card.Key, _.Forget) |> this.Log
    override this.Demote (card: Card) = scheduler.Reschedule(card.Key, _.Demote) |> this.Log
    override this.Keep (card: Card) = scheduler.Reschedule(card.Key, _.Keep) |> this.Log
    override this.Promote (card: Card) = scheduler.Reschedule(card.Key, _.Promote) |> this.Log
namespace Loana.Vocab

open System
open Loana.CLI
open Loana.Data

type LearnSession(cards: Card array, scheduler: ReviewSchedule) =
    inherit StudySession("Learning session", cards)

    override this.Forget (card: Card) = this.ReplaceNear(card)
    override this.Demote (card: Card) = this.ReplaceNear(card)
    override this.Keep (card: Card) = this.ReplaceFar(card)
    override this.Promote (card: Card) = scheduler.Schedule(card.Key, ReviewData.Level1(DateTimeOffset.UtcNow.ToUnixTimeSeconds(), 1)) |> this.Log

type ReviewSession(cards: Card array, scheduler: ReviewSchedule, ahead: bool) =
    inherit StudySession((if ahead then "Review session (Ahead)" else "Review session"), cards)

    override this.Forget (card: Card) = scheduler.Reschedule(card.Key, _.Forget) |> this.Log
    override this.Demote (card: Card) = scheduler.Reschedule(card.Key, _.Demote) |> this.Log
    override this.Keep (card: Card) = scheduler.Reschedule(card.Key, _.Keep) |> this.Log
    override this.Promote (card: Card) = scheduler.Reschedule(card.Key, _.Promote) |> this.Log
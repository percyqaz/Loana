namespace Loana.Vocab

open System
open Loana.CLI
open Loana.Data

type LearnSession(cards: Card array, scheduler: ReviewSchedule) =
    inherit StudySession("Learning session", cards)

    override this.Forget (card: Card) : unit = scheduler.Bury(card.Key) |> this.Log
    override this.Demote (card: Card) : unit = this.ReplaceNear(card)
    override this.Keep (card: Card) : unit = this.ReplaceFar(card)
    override this.Promote (card: Card) : unit =
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        scheduler.Schedule(card.Key, ReviewData.Level1(now, 1), now) |> this.Log

type ReviewSession(cards: Card array, scheduler: ReviewSchedule, ahead: bool) =
    inherit StudySession((if ahead then "Review session (Ahead)" else "Review session"), cards)

    override this.Forget (card: Card) : unit =
        scheduler.Reschedule(card.Key, _.Forget) |> this.Log
    override this.Demote (card: Card) : unit =
        scheduler.Reschedule(card.Key, _.Demote) |> this.Log
    override this.Keep (card: Card) : unit =
        scheduler.Reschedule(card.Key, _.Keep) |> this.Log
        if card.Meta.BumpKey.IsSome then scheduler.Bump(card.Meta) |> this.Log
    override this.Promote (card: Card) : unit =
        scheduler.Reschedule(card.Key, _.Promote) |> this.Log
        if card.Meta.BumpKey.IsSome then scheduler.Bump(card.Meta) |> this.Log
namespace Loana.Vocab

open System
open System.Drawing
open Loana.CLI
open Loana.Data

type LearnSession(cards: CardMeta array, scheduler: ReviewSchedule) =
    inherit StudySession("Learning session", cards)

    override this.Forget (card: CardMeta) : unit =
        scheduler.Bury(card.Key)
        Console.ColorText((sprintf " [L] %s buried!" card.Key).PadRight(MenuRender.Width), Color.LightBlue, Color.FromArgb(0xFF_202020)) |> this.Log
    override this.Demote (card: CardMeta) : unit = this.ReplaceNear(card)
    override this.Keep (card: CardMeta) : unit = this.ReplaceFar(card)
    override this.Promote (card: CardMeta) : unit =
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        scheduler.Schedule(card.Key, ReviewData.Level1(now, 1), now).LogTo this

    override this.Render (card: CardMeta): CardSide * CardSide =
        VocabCard.Render card

type ReviewSession(cards: CardMeta array, scheduler: ReviewSchedule, ahead: bool) =
    inherit StudySession((if ahead then "Review session (Ahead)" else "Review session"), cards)

    override this.Forget (card: CardMeta) : unit =
        scheduler.Reschedule(card.Key, _.Forget).LogTo this
    override this.Demote (card: CardMeta) : unit =
        scheduler.Reschedule(card.Key, _.Demote).LogTo this
    override this.Keep (card: CardMeta) : unit =
        scheduler.Reschedule(card.Key, _.Keep).LogTo this
        if card.BumpKey.IsSome then scheduler.Bump(card).LogTo this
    override this.Promote (card: CardMeta) : unit =
        scheduler.Reschedule(card.Key, _.Promote).LogTo this
        if card.BumpKey.IsSome then scheduler.Bump(card).LogTo this

    override this.Render (card: CardMeta): CardSide * CardSide =
        VocabCard.Render card
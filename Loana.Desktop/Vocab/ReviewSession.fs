namespace Loana.Desktop.Vocab

open System
open System.Drawing
open Loana.Language
open Loana.Data
open Loana.Desktop.CLI

type LearnSession(cards: Card array, scheduler: ReviewSchedule) =
    inherit StudySession("Learning session", cards)

    override this.Forget (card: Card) : unit =
        scheduler.Bury(card.Key)
        Console.ColorText((sprintf " [L] %s buried!" card.Key).PadRight(MenuRender.Width), Color.LightBlue, Color.FromArgb(0xFF_202020)) |> this.Log
    override this.Demote (card: Card) : unit = this.ReplaceNear(card)
    override this.Keep (card: Card) : unit = this.ReplaceFar(card)
    override this.Promote (card: Card) : unit =
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        scheduler.Schedule(card.Key, ReviewData.Level1(now, 1), now).LogTo this

    override this.Render (card: Card): CardSide * CardSide =
        VocabCard.Render card

type ReviewSession(cards: Card array, scheduler: ReviewSchedule, ahead: bool) =
    inherit StudySession((if ahead then "Review session (Ahead)" else "Review session"), cards)

    override this.Forget (card: Card) : unit =
        scheduler.Reschedule(card.Key, _.Forget).LogTo this
    override this.Demote (card: Card) : unit =
        scheduler.Reschedule(card.Key, _.Demote).LogTo this
    override this.Keep (card: Card) : unit =
        scheduler.Reschedule(card.Key, _.Keep).LogTo this
        if card.BumpKey.IsSome then scheduler.Bump(card).LogTo this
    override this.Promote (card: Card) : unit =
        scheduler.Reschedule(card.Key, _.Promote).LogTo this
        if card.BumpKey.IsSome then scheduler.Bump(card).LogTo this

    override this.Render (card: Card): CardSide * CardSide =
        VocabCard.Render card
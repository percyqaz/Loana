namespace Loana.Desktop.Study

open System.Drawing
open Loana.Data
open Loana.Language
open Loana.Desktop.CLI

[<AbstractClass>]
type StudyCardSource() =

    abstract member Next: unit -> Card option
    abstract member Remaining: unit -> int

    abstract member Forgot: Card -> string seq
    abstract member Bad: Card -> string seq
    abstract member Ok: Card -> string seq
    abstract member Good: Card -> string seq

type LearnCardSource(cards: Card array, scheduler: ReviewSchedule) =
    inherit StudyCardSource()

    let cards = ResizeArray(cards |> Seq.randomShuffle)

    override this.Next() : Card option =
        if cards.Count > 0 then
            let next = cards.[0]
            cards.RemoveAt(0)
            Some next
        else
            None

    override this.Remaining() : int = cards.Count

    override this.Forgot(card: Card) : string seq =
        scheduler.Bury(card.Key)
        [ (sprintf " [L] %s buried!" card.Key).ForeColor(Color.LightBlue) ]

    override this.Bad(card: Card) : string seq =
        cards.Insert(min 4 cards.Count, card)
        []

    override this.Ok(card: Card) : string seq =
        cards.Add(card)
        []

    override this.Good(card: Card) : string seq =
        [ scheduler.Learn(card).HighlightString() ]

type ReviewCardSource(cards: Card array, scheduler: ReviewSchedule) =
    inherit StudyCardSource()

    let cards = ResizeArray(cards |> Seq.randomShuffle)

    override this.Next() : Card option =
        if cards.Count > 0 then
            let next = cards.[0]
            cards.RemoveAt(0)
            Some next
        else
            None

    override this.Remaining() : int = cards.Count

    override this.Forgot(card: Card) : string seq =
        [ scheduler.Forget(card).HighlightString() ]

    override this.Bad(card: Card) : string seq =
        [ scheduler.Demote(card).HighlightString() ]

    override this.Ok(card: Card) : string seq =
        scheduler.Keep(card) |> Seq.map _.HighlightString()

    override this.Good(card: Card) : string seq =
        scheduler.Promote(card) |> Seq.map _.HighlightString()

type VerbCardSource(cards: Card array) =
    inherit StudyCardSource()

    let cards = ResizeArray(cards |> Seq.randomShuffle)

    override this.Next() : Card option =
        if cards.Count > 0 then
            let next = cards.[0]
            cards.RemoveAt(0)
            Some next
        else
            None

    override this.Remaining() : int = cards.Count

    override this.Forgot(card: Card) : string seq =
        cards.Insert(min 4 cards.Count, card)
        []

    override this.Bad(card: Card) : string seq =
        cards.Insert(min 4 cards.Count, card)
        []

    override this.Ok(card: Card) : string seq =
        cards.Add(card)
        []

    override this.Good(_card: Card) : string seq = []

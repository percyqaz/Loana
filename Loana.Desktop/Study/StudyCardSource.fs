namespace Loana.Desktop.Study

open System
open System.Drawing
open Loana.Data
open Loana.Language
open Loana.Desktop.CLI
open Loana.Verbs

[<AbstractClass>]
type StudyCardSource() =

    abstract member Next: unit -> Card option
    abstract member Remaining: unit -> int

    abstract member Forgot: Card -> string seq
    abstract member Bad: Card -> string seq
    abstract member Ok: Card -> string seq
    abstract member Good: Card -> string seq

[<Sealed>]
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

[<Sealed>]
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

type CurrentVerb =
    private
        {
            Verb: VerbCacheEntry
            Cards: ResizeArray<Card>
            mutable Mistakes: int
            mutable Forgets: int
        }

[<AbstractClass>]
type VerbCardSource(verbs: VerbCacheEntry array, verb_bank: VerbBank) =
    inherit StudyCardSource()

    let cards_for_verb (verb: VerbCacheEntry) : ResizeArray<Card> =
        verb_bank.EnsureAllInflectionsAvailable(verb.Verb)
        |> Map.toSeq
        |> Seq.filter(fun (i, _) -> i.ToTense = verb.Tense)
        |> Seq.map(fun (i, text) -> VerbCard.Inflection(verb.Verb, i, text))
        |> Seq.randomShuffle
        |> ResizeArray

    let verbs = verbs |> Seq.randomShuffle |> ResizeArray

    let mutable current_verb =
        let first_verb = verbs.[0]
        verbs.RemoveAt(0)

        {
            Verb = first_verb
            Cards = cards_for_verb(first_verb)
            Mistakes = 0
            Forgets = 0
        }

    abstract member DoneWithVerb: CurrentVerb -> string seq

    override this.Next() : Card option =
        if current_verb.Cards.Count > 0 then
            let next = current_verb.Cards.[0]
            current_verb.Cards.RemoveAt(0)
            Some next
        elif verbs.Count > 0 then
            let next_verb = verbs.[0]
            verbs.RemoveAt(0)

            current_verb <-
                {
                    Verb = next_verb
                    Cards = cards_for_verb(next_verb)
                    Mistakes = 0
                    Forgets = 0
                }

            let next_card = current_verb.Cards.[0]
            current_verb.Cards.RemoveAt(0)
            Some next_card
        else
            None

    override this.Remaining() : int =
        verbs.Count + if current_verb.Cards.Count > 0 then 1 else 0

    override this.Forgot(card: Card) : string seq =
        current_verb.Cards.Insert(min 4 current_verb.Cards.Count, card)
        current_verb.Mistakes <- current_verb.Mistakes + 1
        current_verb.Forgets <- current_verb.Forgets + 1
        []

    override this.Bad(card: Card) : string seq =
        current_verb.Cards.Insert(min 4 current_verb.Cards.Count, card)
        current_verb.Mistakes <- current_verb.Mistakes + 1
        []

    override this.Ok(card: Card) : string seq =
        current_verb.Cards.Add(card)
        current_verb.Mistakes <- current_verb.Mistakes + 1
        []

    override this.Good(_card: Card) : string seq =
        if current_verb.Cards.Count = 0 then this.DoneWithVerb(current_verb) else []

[<Sealed>]
type VerbReviewCardSource(verbs: VerbCacheEntry array, verb_bank: VerbBank, scheduler: ReviewSchedule) =
    inherit VerbCardSource(verbs, verb_bank)

    override this.DoneWithVerb(verb: CurrentVerb) : string seq =
        if verb.Mistakes = 0 then [ scheduler.Reschedule(verb.Verb.Key, _.Promote).HighlightString() ]
        elif verb.Mistakes = 1 then [ scheduler.Reschedule(verb.Verb.Key, _.Keep).HighlightString() ]
        elif verb.Forgets > 0 then [ scheduler.Reschedule(verb.Verb.Key, _.Forget).HighlightString() ]
        else [ scheduler.Reschedule(verb.Verb.Key, _.Demote).HighlightString() ]

[<Sealed>]
type VerbLearnCardSource(verbs: VerbCacheEntry array, verb_bank: VerbBank, scheduler: ReviewSchedule) =
    inherit VerbCardSource(verbs, verb_bank)

    override this.DoneWithVerb(verb: CurrentVerb) : string seq =
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()

        [
            scheduler
                .Schedule(verb.Verb.Key, ReviewData.Level1(now, (1 + verb.Mistakes) |> min 10 |> max 1), now)
                .HighlightString()
        ]

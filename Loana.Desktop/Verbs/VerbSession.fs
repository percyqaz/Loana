namespace Loana.Verbs

open Loana.CLI

type VerbSession(cards: CardMeta array) =
    inherit StudySession("Verb practice", cards)

    override this.Forget (card: CardMeta) = this.ReplaceNear(card)
    override this.Demote (card: CardMeta) = this.ReplaceNear(card)
    override this.Keep (card: CardMeta) = this.ReplaceFar(card)
    override this.Promote (_: CardMeta) = ()

    override this.Render (card: CardMeta): CardSide * CardSide =
        match card.Type with
        | Inflection (verb, inflection, inflected_text) -> VerbCard.Inflection(verb, inflection, inflected_text)
        | _ -> failwith "todo: split verb mode off from the rest of the code properly"
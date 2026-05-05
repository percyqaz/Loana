namespace Loana.Desktop.Verbs

open Loana.Language
open Loana.Desktop.CLI

type VerbSession(cards: Card array) =
    inherit StudySession("Verb practice", cards)

    override this.Forget (card: Card) = this.ReplaceNear(card)
    override this.Demote (card: Card) = this.ReplaceNear(card)
    override this.Keep (card: Card) = this.ReplaceFar(card)
    override this.Promote (_: Card) = ()

    override this.Render (card: Card): CardSide * CardSide =
        match card.Type with
        | Inflection (verb, inflection, inflected_text) -> VerbCard.Inflection(verb, inflection, inflected_text)
        | _ -> failwith "todo: split verb mode off from the rest of the code properly"
namespace Loana.Verbs

open Loana.CLI

type VerbSession(cards: Card array) =
    inherit StudySession("Verb practice", cards)

    override this.Forget (card: Card) = this.ReplaceNear(card)
    override this.Demote (card: Card) = this.ReplaceNear(card)
    override this.Keep (card: Card) = this.ReplaceFar(card)
    override this.Promote (_: Card) = ()
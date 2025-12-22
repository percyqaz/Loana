namespace Loana.Decks

open Loana
open Loana.Scheduler

[<AutoOpen>]
module internal ArticleConstants =

    let NOUNS : Noun array =
        [|
            {
                Deutsch = "Löffel"
                English = "spoon"
                Guts = Masculine { Plural = Some "Löffel" }
            }
            {
                Deutsch = "Gabel"
                English = "fork"
                Guts = Feminine { Plural = Some "Gabeln" }
            }
            {
                Deutsch = "Messer"
                English = "knife"
                Guts = Neuter { Plural = Some "Messer" }
            }
            {
                Deutsch = "Löffel"
                English = "spoons"
                Guts = Plural { Singular = Some "Löffel" }
            }
            {
                Deutsch = "Gabeln"
                English = "forks"
                Guts = Plural { Singular = Some "Gabel" }
            }
            {
                Deutsch = "Messer"
                English = "knives"
                Guts = Plural { Singular = Some "Messer" }
            }
        |]

    let KLEIN : Adjective = { Deutsch = "klein"; English = "small" }

[<AbstractClass>]
type Deck() =
    abstract member Name : string
    abstract member Build : CardScheduler -> Card seq

//let DECKS : Map<string, CardPermutation -> bool> = Map.ofList [
//    "[1*] basic 'the'", fun card ->
//        card.Type.IsDefinite && not card.Type.HasAdjective && card.Case.IsNominative
//    "[2*] 'the'", fun card ->
//        card.Type.IsDefinite && not card.Type.HasAdjective
//    "[4*] 'the' + adjective", fun card ->
//        card.Type.IsDefinite && card.Type.HasAdjective
//    "[1*] basic 'a'", fun card ->
//        card.Type.IsIndefinite && not card.Type.HasAdjective && card.Case.IsNominative
//    "[2*] 'a'", fun card ->
//        card.Type.IsIndefinite && not card.Type.HasAdjective
//    "[4*] 'a' + adjective", fun card ->
//        card.Type.IsIndefinite && card.Type.HasAdjective
//    "[3*] 'a' + 'the' combo", fun card ->
//        (card.Type.IsDefinite || card.Type.IsIndefinite) && not card.Type.HasAdjective
//    "[5*] 'a' + 'the' adjective combo", fun card ->
//        (card.Type.IsDefinite || card.Type.IsIndefinite)
//    "[1*] basic personal pronouns", fun card ->
//        card.Type.IsPerson && card.Case.IsNominative
//    "[2*] personal pronouns", fun card ->
//        card.Type.IsPerson
//    "[3*] basic possessive pronouns", fun card ->
//        card.Type.IsPossessive && not card.Type.HasAdjective && card.Case.IsNominative
//    "[4*] possessive pronouns", fun card ->
//        card.Type.IsPossessive && not card.Type.HasAdjective
//    "[6*] possessive pronouns + adjective", fun card ->
//        card.Type.IsPossessive && card.Type.HasAdjective
//    "[4*] tiny bit of everything", fun card ->
//        not card.Type.HasAdjective && card.Case.IsNominative
//    "[6*] tiny bit of everything + adjective", fun card ->
//        (card.Type.HasAdjective || card.Type.IsPerson) && card.Case.IsNominative
//    "[8*] everything", fun card ->
//        card.Type.HasAdjective || card.Type.IsPerson
//
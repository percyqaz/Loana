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

open Avalonia.Media

type EnglishToGermanCard(front: AnnotationTree, back: AnnotationTree, key: string, spacing_rule: CardSpacingRule, scheduler: CardScheduler) =
    inherit Card(key, spacing_rule, scheduler)

    override this.DisplayFront(output: IOutput) : unit =

        AnnotationTree.render(front, output)

        output.Write(" -> English ", AnnotationTree.gradient Colors.Red Colors.Black, Brushes.White)
        output.Write(" ")
        if this.Schedule.LearningStep.IsSome then
            output.WriteLine(" Learning ", Brushes.Black, Brushes.Cyan)

    override this.DisplayBack(output: IOutput): unit =
        AnnotationTree.render(back, output)

    override this.FrontInput(user_input: string, output: IOutput) : CardEase option =
        if user_input = AnnotationTree.flatten_tree back then
            Some CardEase.Okay
        else
            output.WriteLine(" Mistake! See below: ", Brushes.Black, Brushes.Red)
            output.WriteLine(user_input, Brushes.LightPink)
            None

    override this.BackInput(user_input: string, output: IOutput) : CardEase = CardEase.Forgot

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
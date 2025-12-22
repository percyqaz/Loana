namespace Loana.Decks

open Avalonia.Media
open Loana
open Loana.Scheduler
open Loana.Declension

type PossessivePronounsCard(person: Person, adjective: Adjective option, noun: Noun, case: Case, spacing_rule: CardSpacingRule, scheduler: CardScheduler) =
    inherit Card($"""possessive-{person}{(if adjective.IsSome then "-adj" else "")}-{noun}-{case}""", spacing_rule, scheduler)

    let front = English.possessive_fragment person adjective noun case
    let back = Deutsch.possessive_fragment person adjective noun case

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

type PossessivePronounsDeck() =
    inherit Deck()

    let spacing = CardSpacingRule.Familiarise

    override this.Name = "Possessive Pronouns"
    override this.Build(scheduler: CardScheduler) : Card seq =
        seq {
            for person in Person.LIST do
                for noun in NOUNS do
                    for case in Case.LIST do
                        yield PossessivePronounsCard(person, Some KLEIN, noun, case, spacing, scheduler) :> Card
                        yield PossessivePronounsCard(person, None, noun, case, spacing, scheduler)
        } |> Seq.cache
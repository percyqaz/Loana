namespace Loana.Decks

open Avalonia.Media
open Loana
open Loana.Scheduler
open Loana.Declension

type PersonalPronounsCard(person: Person, case: Case, spacing_rule: CardSpacingRule, scheduler: CardScheduler) =
    inherit Card($"personal-pronoun-{person}-{case}", spacing_rule, scheduler)

    let front = English.personal_pronoun person case
    let back = Deutsch.personal_pronoun person case

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

type PersonalPronounsDeck() =
    inherit Deck()

    let spacing = CardSpacingRule.Familiarise

    override this.Name = "Personal Pronouns"
    override this.Build(scheduler: CardScheduler) : Card seq =
        seq {
            for person in Person.LIST do
                for case in [Case.Nominative; Case.Accusative; Case.Dative] do
                    yield PersonalPronounsCard(person, case, spacing, scheduler) :> Card
        } |> Seq.cache
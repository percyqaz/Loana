namespace Loana.Cards

open Avalonia.Media
open Loana
open Loana.Scheduler

type Card(key: string, tags: string list, spacing_rule: CardSpacingRule, scheduler: CardScheduler, front: AnnotationTree, back: AnnotationTree) =
    inherit ICard(key, spacing_rule, scheduler)

    override this.DisplayFront(output: IOutput) : unit =
        output.Clear()
        AnnotationTree.render(front, output)
        output.Write(" -> English ", AnnotationTree.gradient Colors.Red Colors.Black, Brushes.White)
        output.Write(" ")
        if (this :> ICard).Schedule.LearningStep.IsSome then
            output.WriteLine(" Learning ", Brushes.Black, Brushes.Cyan)
        for tag in tags do
            output.Write($" {tag} ", Brushes.Black, Brushes.LightGray)
            output.Write(" ")
        output.WriteLine("")

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
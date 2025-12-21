namespace Loana.Cards

open Avalonia.Media
open Loana
open Loana.Scheduler

type Card(key: string, tags: string list, spacing_rule: CardSpacingRule, scheduler: CardScheduler, front: AnnotationTree, back: AnnotationTree) =

    interface ICard with
        member this.Schedule : CardScheduleData = scheduler.Get(key)

        member this.DisplayFront(output: IOutput) : unit =
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

        member this.DisplayBack(output: IOutput): unit =
            AnnotationTree.render(back, output)

        member this.FrontInput(user_input: string, output: IOutput) : CardEase option =
            if user_input = AnnotationTree.flatten_tree back then
                Some CardEase.Okay
            else
                output.WriteLine(" Mistake! See below: ", Brushes.Black, Brushes.Red)
                output.WriteLine(user_input, Brushes.LightPink)
                None

        member this.BackInput(user_input: string, output: IOutput) : CardEase = CardEase.Forgot

        member this.Reschedule(result: CardEase, now: int64) : unit =
            scheduler.Review(key, spacing_rule, result, now)
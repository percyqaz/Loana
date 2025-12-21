namespace Loana.Cards

open Avalonia.Media
open Loana
open Loana.Scheduler

type Card(front: AnnotationTree, back: AnnotationTree, scheduler: CardSchedule) =

    member this.Key : string =
        AnnotationTree.flatten_tree front

    interface ICard with
        member this.ScheduledTime : int64 = scheduler.Get(this.Key).NextReview

        member this.DisplayFront(output: IOutput) : unit =
            output.Clear()
            AnnotationTree.render(front, output)
            output.Write("-> English", AnnotationTree.gradient Colors.Red Colors.Black, Brushes.White)
            output.Write(" ")
            if scheduler.Get(this.Key).LearningStep.IsSome then
                output.WriteLine("Learning", Brushes.Black, Brushes.Cyan)

        member this.DisplayBack(output: IOutput): unit =
            AnnotationTree.render(back, output)

        member this.FrontInput(user_input: string, output: IOutput) : CardEase option =
            if user_input = AnnotationTree.flatten_tree back then
                Some CardEase.Okay
            else
                output.WriteLine("Mistake! See below:", Brushes.Black, Brushes.Red)
                output.WriteLine(user_input, Brushes.LightPink)
                None

        member this.BackInput(user_input: string, output: IOutput) : CardEase = CardEase.Forgot

        member this.Reschedule(result: CardEase, now: int64) : unit =
            scheduler.Review(this.Key, result, now)
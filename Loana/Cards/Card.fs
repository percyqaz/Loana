namespace Loana.Cards

open System
open Avalonia.Media
open Loana
open Loana.Scheduler

module CardMemory =

    let private mem = Collections.Generic.Dictionary<string, CardHistory>()

    let RULE = CardSpacingRule.Familiarise

    let get_schedule (key: string) : CardHistory =
        match mem.TryGetValue(key) with
        | true, time -> time
        | false, _ -> CardHistory.Initial

    let review (key: string, ease: CardResult, now: int64) =
        mem.[key] <- RULE.Next(get_schedule key, ease, now)
        printfn "Card '%s' reviewed with result %A. Next review at %A" key ease (mem.[key].NextReview)

type Card =
    {
        Front: AnnotationTree
        Back: AnnotationTree
    }

    member this.Key : string =
        AnnotationTree.flatten_tree this.Front

    interface ICard with
        member this.ScheduledTime : int64 = CardMemory.get_schedule(this.Key).NextReview

        member this.DisplayFront(output: IOutput) : unit =
            output.Clear()
            AnnotationTree.render(this.Front, output)
            output.Write("-> English", AnnotationTree.gradient Colors.Red Colors.Black, Brushes.White)
            output.Write(" ")
            if CardMemory.get_schedule(this.Key).LearningStep.IsSome then
                output.WriteLine("Learning", Brushes.Black, Brushes.Cyan)

        member this.DisplayBack(output: IOutput): unit =
            AnnotationTree.render(this.Back, output)

        member this.FrontInput(user_input: string, output: IOutput) : CardResult option =
            if user_input = AnnotationTree.flatten_tree this.Back then
                Some CardResult.Okay
            else
                output.WriteLine("Mistake! See below:", Brushes.Black, Brushes.Red)
                output.WriteLine(user_input, Brushes.LightPink)
                None

        member this.BackInput(user_input: string, output: IOutput) : CardResult = CardResult.Forgot

        member this.Reschedule(result: CardResult, now: int64) : unit =
            CardMemory.review(this.Key, result, now)
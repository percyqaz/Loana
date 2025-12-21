namespace Loana.Cards

open System
open Loana
open Loana.Scheduler

module CardMemory =

    let private mem = Collections.Generic.Dictionary<string, int64>()

    let schedule_for (key: string, time: int64) =
        mem.[key] <- time

    let get_schedule (key: string) : int64 =
        match mem.TryGetValue(key) with
        | true, time -> time
        | false, _ -> 0L

type Card =
    {
        Front: AnnotationTree
        Back: AnnotationTree
    }

    member this.Key : string =
        AnnotationTree.flatten_tree this.Front

    interface ICard with
        member this.ScheduledTime : int64 = CardMemory.get_schedule(this.Key)

        member this.DisplayFront(output: IOutput) : unit =
            output.Clear()
            AnnotationTree.render(this.Front, output)

        member this.DisplayBack(output: IOutput): unit =
            AnnotationTree.render(this.Back, output)

        member this.FrontInput(user_input: string) : CardResult option =
            if user_input = AnnotationTree.flatten_tree this.Back then
                Some CardResult.Okay
            else
                // output.WriteLine(user_input, Brushes.LightPink)
                None

        member this.BackInput(user_input: string) : CardResult = CardResult.Forgot

        member this.Reschedule(result: CardResult, now: int64) : unit =
            match result with
            | CardResult.Forgot
            | CardResult.Bad ->
                CardMemory.schedule_for(this.Key, now + TimeSpan.TicksPerSecond * 10L)
            | CardResult.Okay
            | CardResult.Easy ->
                CardMemory.schedule_for(this.Key, now + TimeSpan.TicksPerMinute * 10L)
namespace Loana.Quizzes

open System
open System.Drawing
open Loana.CLI
open Loana.Data

type QuizScheduler(scheduler: ReviewSchedule) =

    let quizzes = [|
        Articles.DEFINITE
        Articles.INDEFINITE
        Articles.MIXED

        Pronouns.PERSONAL
        Pronouns.REFLEXIVE
        Pronouns.POSSESSIVE
    |]

    member this.Learning() =
        quizzes
        |> Seq.where (fun c -> (scheduler.Get c.Key).IsNone)

    member this.DueReview(now: int64) =
        quizzes
        |> Seq.choose (fun c ->
            match scheduler.Get c.Key with
            | ValueSome data ->
                let dl = data.DueLevel now
                if dl >= 0 then Some (c, dl) else None
            | ValueNone -> None
        )
        |> Seq.sortByDescending snd
        |> Seq.map fst

    member this.AheadReview(now: int64) =
        quizzes
        |> Seq.choose (fun c ->
            match scheduler.Get c.Key with
            | ValueSome data ->
                let n = data.NextReview
                if n > now then Some (c, n) else None
            | ValueNone -> None
        )
        |> Seq.sortBy snd
        |> Seq.map fst

    member this.Study() =

        let mutable loop = true
        while loop do
            Console.Clear()
            Console.WriteLine("Quiz learner :)")
            let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
            for q in this.DueReview(now) do
                Console.WriteLine(" " + q.Name, Color.Green)
            for q in this.Learning() do
                Console.WriteLine(" " + q.Name, Color.LightBlue)
            for q in this.AheadReview(now) do
                Console.WriteLine(" " + q.Name, Color.Yellow)

            match Console.ReadLine() with
            | "ok"
            | "" ->
                let q =
                    Seq.concat [this.DueReview now; this.Learning(); this.AheadReview now]
                    |> Seq.head
                match QuizSession(q.Name, q.Questions()).Start() with
                | None -> ()
                | Some v ->
                    match scheduler.Get q.Key with
                    | ValueNone -> scheduler.Schedule(q.Key, ReviewData.Level1(now, 1)) |> Console.WriteLine
                    | ValueSome d ->
                        if v < 0 then scheduler.Schedule(q.Key, d.Demote now) |> Console.WriteLine
                        elif v > 0 then scheduler.Schedule(q.Key, d.Promote now) |> Console.WriteLine
                        else scheduler.Schedule(q.Key, d.Promote now) |> Console.WriteLine
                    Console.ReadKey(true) |> ignore
            | "back" -> loop <- false
            | _ -> ()
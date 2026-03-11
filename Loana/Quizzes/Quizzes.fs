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

    member this.Quizzes : Quiz seq = quizzes

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

    member this.Auto() : Quiz =
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        Seq.concat [this.DueReview now; this.Learning(); this.AheadReview now]
        |> Seq.head

    member this.Study(quiz: Quiz) =
        match QuizSession(quiz.Name, quiz.Questions()).Start() with
        | None -> ()
        | Some v ->
            let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
            match scheduler.Get quiz.Key with
            | ValueNone -> scheduler.Schedule(quiz.Key, ReviewData.Level1(now, 1), now) |> Console.WriteLine
            | ValueSome _ ->
                if v < 0 then scheduler.Reschedule(quiz.Key, _.Demote) |> Console.WriteLine
                elif v > 0 then scheduler.Reschedule(quiz.Key, _.Promote) |> Console.WriteLine
                else scheduler.Reschedule(quiz.Key, _.Keep) |> Console.WriteLine
            Console.ReadKey(true) |> ignore
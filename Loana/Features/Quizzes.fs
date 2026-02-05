namespace Loana.Features

open System
open System.Drawing
open Loana.CLI
open Loana.Study

type Quiz =
    {
        Name: string
        Key: string
        Study: unit -> unit
    }

type QuizScheduler(scheduler: ReviewSchedule) =

    let quizzes = [|
        {
            Name = "Articles"
            Key = "quiz-articles"
            Study = fun () -> ArticlesQuiz().Study()
        }
        {
            Name = "Personal Pronouns"
            Key = "quiz-personal-pronouns"
            Study = fun () -> PersonalPronounsQuiz().Study()
        }
        {
            Name = "Reflexive Pronouns"
            Key = "quiz-reflexive-pronouns"
            Study = fun () -> ReflexivePronounsQuiz().Study()
        }
        {
            Name = "Possessive Pronouns"
            Key = "quiz-possessive-pronouns"
            Study = fun () -> PossessivePronounsQuiz().Study()
        }
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
                q.Study()
                match scheduler.Get q.Key with
                | ValueNone -> scheduler.Schedule(q.Key, ReviewData.Level1(now, 1))
                | ValueSome d -> scheduler.Schedule(q.Key, d.Promote now)
            | "back" -> loop <- false
            | _ -> ()
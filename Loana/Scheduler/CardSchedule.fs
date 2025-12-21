namespace Loana.Scheduler

open System

type CardHistory =
    {
        Reviews: int
        Streak: int
        LearningStep: int option
        LastReviewed: int64 option
        NextReview: int64
        Interval: int64
    }

    static member Initial : CardHistory =
        {
            Reviews = 0
            LearningStep = Some 0
            Streak = 0
            LastReviewed = None
            NextReview = 0L
            Interval = CardHistory.DEFAULT_INTERVAL
        }

    static member DEFAULT_INTERVAL: int64 = TimeSpan.TicksPerSecond * 10L

type CardSpacingRule =
    {
        LearningSteps: int64 array
        GraduatingInterval: int64
        Fuzz: int64 -> int64
        Bad: int64 -> int64
        Okay: int64 -> int64
        Easy: int64 -> int64
    }

    static member HighRetention : CardSpacingRule =
        let rand = Random()
        let multiply (factor: float) (interval: int64) : int64 =
            int64 (float interval * factor)

        let fuzz (interval: int64) : int64 =
            multiply (1.0 + (rand.NextDouble() * 0.2 - 0.1)) interval
        {
            LearningSteps =
                [|
                    TimeSpan.TicksPerSecond * 10L
                    TimeSpan.TicksPerSecond * 30L
                    TimeSpan.TicksPerMinute * 1L
                    TimeSpan.TicksPerMinute * 2L
                    TimeSpan.TicksPerMinute * 5L
                    TimeSpan.TicksPerMinute * 10L
                |]
            GraduatingInterval = TimeSpan.TicksPerHour * 12L
            Fuzz = fuzz
            Bad = multiply 0.5 >> min (TimeSpan.TicksPerDay * 3L)
            Okay = multiply 1.1
            Easy = multiply 1.6
        }

    static member Familiarise : CardSpacingRule =
        let rand = Random()
        let multiply (factor: float) (interval: int64) : int64 =
            int64 (float interval * factor)

        let fuzz (interval: int64) : int64 =
            multiply (1.0 + (rand.NextDouble() * 0.2 - 0.1)) interval
        {
            LearningSteps =
                [|
                    TimeSpan.TicksPerMinute * 2L
                |]
            GraduatingInterval = TimeSpan.TicksPerHour * 3L
            Fuzz = fuzz
            Bad = multiply 0.5
            Okay = multiply 1.5
            Easy = multiply 2.0
        }

    member private this.NextLearning(history: CardHistory, ease: CardEase, now: int64, step: int): CardHistory =
        if step >= this.LearningSteps.Length then
            match ease with
            | CardEase.Forgot ->
                { history with
                    LearningStep = Some 0
                    Streak = 0
                    NextReview = now + this.Fuzz (Array.tryHead this.LearningSteps |> Option.defaultValue 0L)
                }

            | CardEase.Bad ->
                { history with
                    LearningStep = None
                    NextReview = now + this.Fuzz this.GraduatingInterval
                    Interval = this.GraduatingInterval
                }

            | CardEase.Okay
            | CardEase.Easy ->

                { history with
                    Streak = history.Streak + 1
                    LearningStep = None
                    NextReview = now + this.Fuzz this.GraduatingInterval
                    Interval = this.GraduatingInterval
                }
        else
            match ease with
            | CardEase.Forgot ->
                { history with
                    LearningStep = Some 0
                    Streak = 0
                    NextReview = now + (Array.tryHead this.LearningSteps |> Option.defaultValue 0L)
                }

            | CardEase.Bad ->
                { history with
                    NextReview = now + this.Fuzz history.Interval
                }

            | CardEase.Okay ->
                { history with
                    Streak = history.Streak + 1
                    LearningStep = Some (step + 1)
                    NextReview = now + this.Fuzz this.LearningSteps.[step]
                }

            | CardEase.Easy ->
                { history with
                    Streak = history.Streak + 1
                    LearningStep = None
                    NextReview = now + this.Fuzz this.GraduatingInterval
                    Interval = this.GraduatingInterval
                }

    member this.Next(history: CardHistory, ease: CardEase, now: int64) : CardHistory =

        let history = { history with LastReviewed = Some now; Reviews = history.Reviews + 1 }

        match history.LearningStep with
        | Some step -> this.NextLearning(history, ease, now, step)
        | None ->
            match ease with
            | CardEase.Forgot ->
                { history with
                    LearningStep = Some 0
                    Streak = 0
                    NextReview = now + this.Fuzz (Array.tryHead this.LearningSteps |> Option.defaultValue 0L)
                }

            | CardEase.Bad ->
                let interval = this.Bad history.Interval
                { history with
                    NextReview = now + this.Fuzz interval
                    Interval = interval
                }

            | CardEase.Okay ->
                let interval = this.Okay history.Interval
                { history with
                    Streak = history.Streak + 1
                    NextReview = now + this.Fuzz interval
                    Interval = interval
                }

            | CardEase.Easy ->
                { history with
                    Streak = history.Streak + 1
                    LearningStep = None
                    NextReview = now + this.Fuzz this.GraduatingInterval
                    Interval = this.GraduatingInterval
                }

type NoteHistory =
    {
        Level: int
    }
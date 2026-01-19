namespace Loana.Scheduler

open System
open System.Text
open System.IO
open System.Collections.Generic
open Avalonia.Media
open Loana.Interface

[<RequireQualifiedAccess>]
type CardEase =
    | Forgot
    | Bad
    | Okay
    | Easy

type CardScheduleData =
    {
        Reviews: int
        Streak: int
        LearningStep: int option
        LastReviewed: int64 option
        NextReview: int64
        Interval: int64
    }

    static member Initial : CardScheduleData =
        {
            Reviews = 0
            LearningStep = Some 0
            Streak = 0
            LastReviewed = None
            NextReview = 0L
            Interval = CardScheduleData.DEFAULT_INTERVAL
        }

    static member DEFAULT_INTERVAL: int64 = 10L

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
                    10L
                    30L
                    TimeSpan.SecondsPerMinute * 1L
                    TimeSpan.SecondsPerMinute * 2L
                    TimeSpan.SecondsPerMinute * 5L
                    TimeSpan.SecondsPerMinute * 10L
                |]
            GraduatingInterval = TimeSpan.SecondsPerHour * 12L
            Fuzz = fuzz
            Bad = multiply 0.5 >> min (TimeSpan.SecondsPerDay * 3L)
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
                    TimeSpan.SecondsPerMinute * 2L
                |]
            GraduatingInterval = TimeSpan.SecondsPerHour * 3L
            Fuzz = fuzz
            Bad = multiply 0.5
            Okay = multiply 1.5
            Easy = multiply 2.0
        }

    member private this.NextLearning(history: CardScheduleData, ease: CardEase, now: int64, step: int): CardScheduleData =
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

    member this.Next(history: CardScheduleData, ease: CardEase, now: int64) : CardScheduleData =

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

type CardSchedulerFile(path: string) =

    let VERSION = 1

    member private this.ReadCardEntry(version: int, br: BinaryReader) : string * CardScheduleData =
        if version <> VERSION then failwithf "Unsupported version '%i'" version
        let id = br.ReadString()
        let data : CardScheduleData =
            {
                Reviews = br.ReadInt32()
                Streak = br.ReadInt32()
                LearningStep = if br.ReadByte() > 0uy then Some(br.ReadInt32()) else None
                LastReviewed = if br.ReadByte() > 0uy then Some(br.ReadInt64()) else None
                NextReview = br.ReadInt64()
                Interval = br.ReadInt64()
            }
        id, data

    member private this.WriteCardEntry(id: string, data: CardScheduleData, bw: BinaryWriter) =
        bw.Write id
        bw.Write data.Reviews
        bw.Write data.Streak
        match data.LearningStep with
        | None -> bw.Write 0uy
        | Some l -> bw.Write 1uy; bw.Write l
        match data.LastReviewed with
        | None -> bw.Write 0uy
        | Some l -> bw.Write 1uy; bw.Write l
        bw.Write data.NextReview
        bw.Write data.Interval

    member this.Load() : Dictionary<string, CardScheduleData> =
        let stream = File.Open(path, FileMode.OpenOrCreate)
        use br = new BinaryReader(stream, Encoding.UTF8, leaveOpen = false)

        if stream.Position = stream.Length then
            printfn "Schedule file '%s' is empty" path
            Dictionary()
        else

        let version = br.ReadInt32()
        let count = br.ReadInt32()
        try
            seq {
                for _ = 1 to count do
                    yield this.ReadCardEntry(version, br)
            }
            |> Seq.map KeyValuePair
            |> Dictionary
        with
        | :? EndOfStreamException -> reraise()

    member this.Write(data: Dictionary<string, CardScheduleData>) =
        let bak_path = path + ".bak"
        let temp_path = path + ".tmp"
        let stream = File.Open(temp_path, FileMode.Create)
        let bw = new BinaryWriter(stream, Encoding.UTF8, leaveOpen = false)
        bw.Write(VERSION)
        bw.Write(data.Count)
        for kvp in data do
            this.WriteCardEntry(kvp.Key, kvp.Value, bw)
        bw.Dispose()

        try File.Delete(bak_path) with _ -> ()
        File.Move(path, bak_path)
        File.Move(temp_path, path)

type CardScheduler(path: string, output: IOutput) =

    let db = CardSchedulerFile(path)
    let mem = db.Load()

    member this.Get(key: string) : CardScheduleData =
        match mem.TryGetValue(key) with
        | true, time -> time
        | false, _ -> CardScheduleData.Initial

    member this.Review(key: string, spacing_rule: CardSpacingRule, ease: CardEase, now: int64) =
        let new_schedule = spacing_rule.Next(this.Get key, ease, now)
        mem.[key] <- new_schedule
        let ease_color =
            match ease with
            | CardEase.Forgot -> Brushes.DarkRed
            | CardEase.Bad -> Brushes.OrangeRed
            | CardEase.Okay -> Brushes.YellowGreen
            | CardEase.Easy -> Brushes.Green
        output.Write(sprintf "%A" ease, ease_color)
        output.Write(", ")
        match new_schedule.LearningStep with
        | Some step -> output.WriteLine(sprintf "'%s' -> %O (%i/%i)" key (DateTimeOffset.FromUnixTimeSeconds(new_schedule.NextReview)) step spacing_rule.LearningSteps.Length, Brushes.LightBlue)
        | None -> output.WriteLine(sprintf "'%s' -> %O" key (DateTimeOffset.FromUnixTimeSeconds(new_schedule.NextReview)))
        db.Write(mem)

type NoteHistory =
    {
        Level: int
    }
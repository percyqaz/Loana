namespace Loana.Data

open System
open System.Text
open System.IO
open System.Drawing
open System.Collections.Generic
open Loana.CLI

type ScheduleResult =
    {
        Key: string
        OldLevel: int
        NewLevel: int
        Difficulty: int
        Interval: int64
    }

[<RequireQualifiedAccess>]
type ReviewEase =
    | Forgot
    | Poor
    | Okay
    | Easy

type ReviewData =
    {
        Reviews: int
        Level: int
        Difficulty: int
        LastReviewed: int64
        Interval: int64
    }

    static let level_colors = [|
            Color.FromArgb(0xF0_709090)
            Color.FromArgb(0xF0_709070)
            Color.FromArgb(0xF0_509050)
            Color.FromArgb(0xF0_309030)
            Color.FromArgb(0xF0_109010)
            Color.FromArgb(0xF0_008000)
            Color.FromArgb(0xF0_006000)
            Color.FromArgb(0xF0_004000)
            Color.FromArgb(0xF0_002000)
        |]
    static member LevelColors = level_colors

    static member private GetNextInterval(level: int, difficulty: int, current_interval: int64, overdue_by: int64) : int64 =

        if level = 8 then
            let scale = 16 - difficulty
            current_interval * int64 scale / 10L
        else

        let tenths = 11 - difficulty
        let fuzz = int64 <| Random().Next(int TimeSpan.SecondsPerMinute * 30)
        let base_interval =
            match level with
            | 1 -> TimeSpan.SecondsPerHour * 6L
            | 2 -> TimeSpan.SecondsPerHour * 20L
            | 3 -> TimeSpan.SecondsPerDay * 3L
            | 4 -> TimeSpan.SecondsPerDay * 7L
            | 5 -> TimeSpan.SecondsPerDay * 14L
            | 6 -> TimeSpan.SecondsPerDay * 28L
            | 7 -> TimeSpan.SecondsPerDay * 90L
            | _ -> failwithf "level %i out of range" level
        base_interval * int64 tenths / 10L - fuzz + max 0L overdue_by

    static member Level1(now: int64, difficulty: int) : ReviewData =
        let difficulty = difficulty |> max 1 |> min 10
        {
            Reviews = 0
            Level = 1
            Difficulty = difficulty |> max 1 |> min 10
            LastReviewed = now
            Interval = ReviewData.GetNextInterval(1, difficulty, 10L, 0L)
        }

    static member SeedAtLevel(now: int64, level: int) : ReviewData =
        let level = level |> max 1 |> min 8
        let interval = ReviewData.GetNextInterval(level, 5, 10L, 0L)
        {
            Reviews = 0
            Level = level
            Difficulty = 5
            LastReviewed = now - int64 (Random().Next(int interval))
            Interval = interval
        }

    member this.NextReview : int64 = this.LastReviewed + this.Interval

    member this.DueLevel(now: int64) : int =
        let amount_overdue = now - this.NextReview
        if amount_overdue < 0L then -1 else float32 amount_overdue / float32 (max 1L this.Interval) * 10000f |> floor |> int

    member this.Forget(now: int64) : ReviewData =
        let difficulty = this.Difficulty + 5 |> min 10
        {
            Reviews = this.Reviews + 1
            Level = 1
            Difficulty = difficulty
            LastReviewed = now
            Interval = ReviewData.GetNextInterval(1, difficulty, this.Interval, 0L)
        }

    member this.Demote(now: int64) : ReviewData =
        let level = this.Level - 1 |> max 1 |> min 5
        let difficulty = this.Difficulty + 3 |> min 10
        {
            Reviews = this.Reviews + 1
            Level = level
            Difficulty = difficulty
            LastReviewed = now
            Interval = ReviewData.GetNextInterval(level, difficulty, this.Interval, 0L)
        }

    member this.Keep(now: int64) : ReviewData =
        let difficulty = this.Difficulty + 1 |> min 10
        {
            Reviews = this.Reviews + 1
            Level = this.Level
            Difficulty = difficulty
            LastReviewed = now
            Interval = ReviewData.GetNextInterval(this.Level, difficulty, this.Interval, 0L)
        }

    member this.Promote(now: int64) : ReviewData =
        let level = this.Level + 1 |> min 8
        let difficulty = this.Difficulty - 1 |> max 1
        {
            Reviews = this.Reviews + 1
            Level = level
            Difficulty = difficulty
            LastReviewed = now
            Interval = ReviewData.GetNextInterval(level, difficulty, this.Interval, now - this.NextReview)
        }

    member this.Bump(now: int64, parent_interval: int64) : ReviewData =
        let new_next_review = this.NextReview + TimeSpan.SecondsPerDay + parent_interval / 2L
        let now_plus_day = now + TimeSpan.SecondsPerDay
        let new_interval = max now_plus_day new_next_review - this.LastReviewed
        let difficulty = this.Difficulty - 1 |> max 1
        { this with
            Difficulty = difficulty
            Interval = new_interval
        }

type ReviewScheduleFile(path: string) =

    let VERSION = 2

    member private this.ReadCardEntry(version: int, br: BinaryReader) : string * ReviewData =
        if version <> VERSION then failwithf "Unsupported version '%i'" version
        let id = br.ReadString()
        let data : ReviewData =
            {
                Reviews = br.ReadInt32()
                Level = br.ReadByte() |> int
                Difficulty = br.ReadByte() |> int
                LastReviewed = br.ReadInt64()
                Interval = br.ReadInt64()
            }
        id, data

    member private this.WriteCardEntry(id: string, data: ReviewData, bw: BinaryWriter) : unit =
        bw.Write id
        bw.Write data.Reviews
        bw.Write (byte data.Level)
        bw.Write (byte data.Difficulty)
        bw.Write data.LastReviewed
        bw.Write data.Interval

    member this.Load() : Dictionary<string, ReviewData> =
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

    member this.Write(data: Dictionary<string, ReviewData>) : unit =
        let bak_path = path + ".bak"
        let temp_path = path + ".tmp"
        let stream = File.Open(temp_path, FileMode.Create)
        let bw = new BinaryWriter(stream, Encoding.UTF8, leaveOpen = false)
        bw.Write(VERSION)
        bw.Write(data.Count)
        for kvp in data do
            this.WriteCardEntry(kvp.Key, kvp.Value, bw)
        bw.Dispose()

        try
            File.Delete(bak_path)
            File.Move(path, bak_path)
            File.Move(temp_path, path)
        with err ->
            Console.WriteLine(err.Message)

type ReviewSchedule(path: string) =

    let db = ReviewScheduleFile(path)
    let mem = db.Load()

    let mutable buried: Set<string> = Set.empty

    member this.Get(key: string) : ReviewData voption =
        match mem.TryGetValue(key) with
        | true, data -> ValueSome data
        | false, _ -> ValueNone

    member this.IsBuried(key: string) : bool = buried.Contains(key)

    member this.Bury(key: string) : unit =
        buried <- buried.Add key

    member this.Schedule(key: string, data: ReviewData, now: int64) : ScheduleResult =
        let old_level = this.Get key |> ValueOption.map _.Level |> ValueOption.defaultValue 0
        mem.[key] <- data
        db.Write(mem)
        {
            Key = key
            OldLevel = old_level
            NewLevel = data.Level
            Difficulty = data.Difficulty
            Interval = data.NextReview - now
        }

    member this.Reschedule(key: string, f: ReviewData -> int64 -> ReviewData) : ScheduleResult =
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        this.Schedule(key, f <| this.Get(key).Value <| now, now)

    member this.Bump(meta: Card) : ScheduleResult =
        this.Reschedule(meta.BumpKey.Value, fun data now -> data.Bump(now, this.Get(meta.Key).Value.Interval))
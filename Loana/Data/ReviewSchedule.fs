namespace Loana.Data

open System
open System.Text
open System.IO
open System.Drawing
open System.Collections.Generic
open Loana.Language

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

    static let level_colors =
        [|
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

    static member BaseInterval(level: int) =
        match level with
        | 1 -> TimeSpan.SecondsPerHour * 6L
        | 2 -> TimeSpan.SecondsPerDay * 2L
        | 3 -> TimeSpan.SecondsPerDay * 7L
        | 4 -> TimeSpan.SecondsPerDay * 15L
        | 5 -> TimeSpan.SecondsPerDay * 30L
        | 6 -> TimeSpan.SecondsPerDay * 50L
        | 7 -> TimeSpan.SecondsPerDay * 90L
        | _ -> failwithf "level %i out of range" level

    static member private GetNextInterval
        (level: int, difficulty: int, current_interval: int64, overdue_by: int64)
        : int64 =

        if level = 8 then
            let scale = 16 - difficulty
            current_interval * int64 scale / 10L
        else

        let tenths = 11 - difficulty
        let fuzz = int64 <| Random().Next(int TimeSpan.SecondsPerMinute * 30)
        ReviewData.BaseInterval level * int64 tenths / 10L - fuzz + max 0L overdue_by

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
            LastReviewed = now - int64(Random().Next(int interval))
            Interval = interval
        }

    member this.NextReview: int64 = this.LastReviewed + this.Interval

    member this.OverduePriority(now: int64) : int =
        let amount_overdue = now - this.NextReview

        if amount_overdue < 0L then
            -1
        else
            float32 amount_overdue / float32(max 1L this.Interval) * 10000f |> floor |> int

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
        let level = this.Level - 1 |> max 1 |> min 4
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
        let new_next_review =
            this.NextReview + TimeSpan.SecondsPerDay + parent_interval / 2L

        let now_plus_day = now + TimeSpan.SecondsPerDay
        let new_interval = max now_plus_day new_next_review - this.LastReviewed
        let difficulty = this.Difficulty - 1 |> max 1
        { this with Difficulty = difficulty; Interval = new_interval }

type ReviewScheduleFile(path: string) =

    [<Literal>]
    static let VERSION = 2

    static member ReadFromStream(stream: Stream) : Dictionary<string, ReviewData> =
        use br = new BinaryReader(stream, Encoding.UTF8, leaveOpen = false)

        let version = br.ReadInt32()

        if version <> VERSION then
            failwithf "Unsupported version '%i'" version

        let entry_count = br.ReadInt32()
        let output = Dictionary<string, ReviewData>(entry_count)

        let read_entry () : unit =
            let id = br.ReadString()

            let data: ReviewData =
                {
                    Reviews = br.ReadInt32()
                    Level = br.ReadByte() |> int
                    Difficulty = br.ReadByte() |> int
                    LastReviewed = br.ReadInt64()
                    Interval = br.ReadInt64()
                }

            output.Add(id, data)

        for _ = 1 to entry_count do
            read_entry()

        output

    member this.ReadFromFile() : Dictionary<string, ReviewData> =
        let stream = File.Open(path, FileMode.OpenOrCreate)

        if stream.Position = stream.Length then
            printfn "Schedule file '%s' is empty" path
            Dictionary()
        else
            ReviewScheduleFile.ReadFromStream(stream)

    static member WriteToStream(entries: IReadOnlyDictionary<string, ReviewData>, stream: Stream) : unit =
        let bw = new BinaryWriter(stream, Encoding.UTF8, leaveOpen = true)
        bw.Write(VERSION)
        bw.Write(entries.Count)

        let write_entry (id: string, data: ReviewData) : unit =
            bw.Write id
            bw.Write data.Reviews
            bw.Write(byte data.Level)
            bw.Write(byte data.Difficulty)
            bw.Write data.LastReviewed
            bw.Write data.Interval

        for kvp in entries do
            write_entry(kvp.Key, kvp.Value)

        bw.Dispose()

    member this.WriteToFile(data: IReadOnlyDictionary<string, ReviewData>) : unit =
        let bak_path = path + ".bak"
        let temp_path = path + ".tmp"
        let stream = File.Open(temp_path, FileMode.Create)
        ReviewScheduleFile.WriteToStream(data, stream)
        stream.Dispose()

        try
            File.Delete(bak_path)
            File.Move(path, bak_path)
            File.Move(temp_path, path)
        with err ->
            Console.WriteLine(err.Message)

type ReviewSchedule(path: string) =

    let db = ReviewScheduleFile(path)
    let schedule_data = db.ReadFromFile()

    let mutable buried: Set<string> = Set.empty

    member this.Save() = db.WriteToFile(schedule_data)

    member this.SaveDebounced() =
        // todo: save if not saved in 30s
        // todo: ignore errors
        this.Save()

    member this.Get(key: string) : ReviewData voption =
        match schedule_data.TryGetValue(key) with
        | true, data -> ValueSome data
        | false, _ -> ValueNone

    member this.IsBuried(key: string) : bool = buried.Contains(key)

    member this.Bury(key: string) : unit = buried <- buried.Add key

    member this.Schedule(key: string, data: ReviewData, now: int64) : ScheduleResult =
        let old_level =
            this.Get key |> ValueOption.map _.Level |> ValueOption.defaultValue 0

        schedule_data.[key] <- data
        this.SaveDebounced()

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

    member this.Data = schedule_data.AsReadOnly()

    member this.SyncWith(other_data: IReadOnlyDictionary<string, ReviewData>) : int =
        let mutable updates = 0

        for key in other_data.Keys do
            if schedule_data.ContainsKey(key) then
                let existing = schedule_data.[key]
                let incoming = other_data.[key]

                if incoming.LastReviewed > existing.LastReviewed then
                    schedule_data.[key] <- incoming
                    updates <- updates + 1
            else
                schedule_data.[key] <- other_data.[key]
                updates <- updates + 1

        this.Save()
        updates

    member private this.Bump(card: Card) : ScheduleResult =
        this.Reschedule(card.BumpKey.Value, (fun data now -> data.Bump(now, this.Get(card.Key).Value.Interval)))

    member this.Learn(card: Card) : ScheduleResult =
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        this.Schedule(card.Key, ReviewData.Level1(now, 1), now)

    member this.Forget(card: Card) : ScheduleResult = this.Reschedule(card.Key, _.Forget)

    member this.Demote(card: Card) : ScheduleResult = this.Reschedule(card.Key, _.Demote)

    member this.Keep(card: Card) : ScheduleResult seq =
        seq {
            yield this.Reschedule(card.Key, _.Keep)

            if card.BumpKey.IsSome then
                yield this.Bump(card)
        }
        |> Seq.toArray
        |> Array.toSeq

    member this.Promote(card: Card) : ScheduleResult seq =
        seq {
            yield this.Reschedule(card.Key, _.Promote)

            if card.BumpKey.IsSome then
                yield this.Bump(card)
        }
        |> Seq.toArray
        |> Array.toSeq

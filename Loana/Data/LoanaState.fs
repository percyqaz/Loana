namespace Loana.Data

open System.IO

type LoanaState =
    {
        Path: string
        Scheduler: ReviewSchedule
        Words: WordBank
        Verbs: VerbBank
    }

    static member Create(path: string) : LoanaState =
        {
            Path = path
            Scheduler = ReviewSchedule(Path.Combine(path, "cards.dat"))
            Words = WordBank.CreateFromDirectory(path)
            Verbs = VerbBank(Path.Combine(path, "verbs.verblist"))
        }

    member this.Reload() : unit = this.Words.ReadFromDirectory(this.Path)

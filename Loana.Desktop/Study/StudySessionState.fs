namespace Loana.Desktop.Study

open Loana.Data
open Loana.Language
open Loana.Desktop.CLI

type StudySessionState =
    {
        mutable Running: bool
        UIContext: UIContext
        Title: string
        Cards: StudyCardSource
        Log: ResizeArray<string>
        mutable Forgot: int
        mutable Bad: int
        mutable Ok: int
        mutable Good: int
    }

    static member val private SharedLog = ResizeArray<string>()

    static member Create(title: string, source: StudyCardSource, ui_ctx: UIContext) : StudySessionState =
        {
            Running = source.Remaining() > 0
            UIContext = ui_ctx
            Title = title
            Cards = source
            Log = StudySessionState.SharedLog
            Forgot = 0
            Bad = 0
            Ok = 0
            Good = 0
        }

    static member VerbMode(cards: Card array, ui_ctx: UIContext) : StudySessionState =
        StudySessionState.Create("Verb practice", VerbCardSource(cards), ui_ctx)

    static member Review(cards: Card array, scheduler: ReviewSchedule, ui_ctx: UIContext) : StudySessionState =
        StudySessionState.Create("Review session", ReviewCardSource(cards, scheduler), ui_ctx)

    static member Learn(cards: Card array, scheduler: ReviewSchedule, ui_ctx: UIContext) : StudySessionState =
        StudySessionState.Create("Learn session", LearnCardSource(cards, scheduler), ui_ctx)

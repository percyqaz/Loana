namespace Loana.Desktop.Study

open System.Drawing
open Loana.Data
open Loana.Language
open Loana.Desktop.CLI
open Loana.Verbs

type StudySessionCardState =
    | Front of Card
    | Back of Card

type StudySessionState =
    {
        mutable Running: bool
        UIContext: UIContext
        Title: string
        Cards: StudyCardSource
        mutable CardState: StudySessionCardState
        Log: ResizeArray<string>
        mutable ForgotCount: int
        mutable BadCount: int
        mutable OkCount: int
        mutable GoodCount: int
    }

    member this.LogMessage(message: string) : unit =
        let LOG_SIZE = 16

        let message =
            message.PadRight(MenuRender.Width).BackColor(Color.FromArgb(0xFF_202020))

        this.Log.Add(message)

        if this.Log.Count > LOG_SIZE then
            this.Log.RemoveAt(0)

    member this.LogMessage(result: ScheduleResult) : unit =
        this.LogMessage(result.HighlightString())

    static member val private SharedLog = ResizeArray<string>()

    static member Create(title: string, source: StudyCardSource, ui_ctx: UIContext) : StudySessionState =
        {
            Running = source.Remaining() > 0
            UIContext = ui_ctx
            Title = title
            Cards = source
            CardState = Front(source.Next().Value)
            Log = StudySessionState.SharedLog
            ForgotCount = 0
            BadCount = 0
            OkCount = 0
            GoodCount = 0
        }

    static member VerbReview
        (verbs: VerbCacheEntry array, verb_bank: VerbBank, scheduler: ReviewSchedule, ui_ctx: UIContext)
        : StudySessionState =
        StudySessionState.Create("Verb practice", VerbReviewCardSource(verbs, verb_bank, scheduler), ui_ctx)

    static member VerbLearn
        (verb: VerbCacheEntry, verb_bank: VerbBank, scheduler: ReviewSchedule, ui_ctx: UIContext)
        : StudySessionState =
        StudySessionState.Create("Verb practice", VerbLearnCardSource([| verb |], verb_bank, scheduler), ui_ctx)

    static member Review(cards: Card array, scheduler: ReviewSchedule, ui_ctx: UIContext) : StudySessionState =
        StudySessionState.Create("Review session", ReviewCardSource(cards, scheduler), ui_ctx)

    static member Learn(cards: Card array, scheduler: ReviewSchedule, ui_ctx: UIContext) : StudySessionState =
        StudySessionState.Create("Learn session", LearnCardSource(cards, scheduler), ui_ctx)

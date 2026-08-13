namespace Loana.Desktop.Study

open Loana.Language
open Loana.Desktop.CLI

type StudySessionState =
    {
        mutable Running: bool
        UIContext: UIContext
        Title: string
        Cards: ResizeArray<Card>
        Log: ResizeArray<string>
        mutable Forgot: int
        mutable Bad: int
        mutable Ok: int
        mutable Good: int
    }

    static member val private SharedLog = ResizeArray<string>()

    static member Create(title: string, cards: Card array, ui_ctx: UIContext) : StudySessionState =
        {
            Running = cards.Length > 0
            UIContext = ui_ctx
            Title = title
            Cards = ResizeArray(cards |> Seq.randomShuffle)
            Log = StudySessionState.SharedLog
            Forgot = 0
            Bad = 0
            Ok = 0
            Good = 0
        }

    static member VerbMode(cards: Card array, ui_ctx: UIContext) : StudySessionState =
        StudySessionState.Create("Verb practice", cards, ui_ctx)

    static member Review(cards: Card array, ui_ctx: UIContext) : StudySessionState =
        StudySessionState.Create("Review session", cards, ui_ctx)

    static member Learn(cards: Card array, ui_ctx: UIContext) : StudySessionState =
        StudySessionState.Create("Learn session", cards, ui_ctx)

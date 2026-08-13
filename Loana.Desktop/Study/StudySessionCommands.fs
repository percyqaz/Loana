namespace Loana.Desktop.Study

open System.Runtime.CompilerServices

type StudySessionCommands =

    [<Extension>]
    static member Exit(state: StudySessionState) : unit = state.Running <- false

    [<Extension>]
    static member Echo(state: StudySessionState, text: string) : unit = state.UIContext.StatusLine <- text

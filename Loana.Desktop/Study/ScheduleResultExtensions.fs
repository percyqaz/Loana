namespace Loana.Desktop.Study

open System.Drawing
open System.Runtime.CompilerServices
open Loana.Desktop.CLI
open Loana.Data

type ScheduleResultExtensions =

    [<Extension>]
    static member HighlightString(result: ScheduleResult) : string =
        (sprintf " [%i] %s" result.OldLevel result.Key)
            .PadRight(MenuRender.Width - 52)
            .Substring(0, MenuRender.Width - 52)
            .ForeColor(ReviewData.LevelColors.[result.OldLevel])
        + " -> ".ForeColor(Color.LightGray)
        + (sprintf " Level %i " result.NewLevel).ForeColor(ReviewData.LevelColors.[result.NewLevel])
        + $" Difficulty {result.Difficulty.ToString().PadRight(2)} "
            .ForeColor(if result.Difficulty >= 5 then Color.Red else Color.LightGray)
        + $" Next review: {MenuRender.FormatInterval(result.Interval)} ".ForeColor(Color.LightGreen)

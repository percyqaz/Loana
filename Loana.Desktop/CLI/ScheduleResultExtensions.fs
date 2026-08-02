namespace Loana.Desktop.CLI

open System.Drawing
open System.Runtime.CompilerServices
open Loana.Desktop.CLI
open Loana.Data

type OutputResultExtensions =

    [<Extension>]
    static member HighlightString(result: ScheduleResult) : string =
        Console.ColorText(
            (sprintf " [%i] %s" result.OldLevel result.Key)
                .PadRight(MenuRender.Width - 52)
                .Substring(0, MenuRender.Width - 52),
            ReviewData.LevelColors.[result.OldLevel],
            Color.FromArgb(0xFF_202020)
        )
        + Console.ColorText(" -> ", Color.LightGray, Color.FromArgb(0xFF_202020))
        + Console.ColorText(
            sprintf " Level %i " result.NewLevel,
            ReviewData.LevelColors.[result.NewLevel],
            Color.FromArgb(0xFF_202020)
        )
        + Console.ColorText(
            $" Difficulty {result.Difficulty.ToString().PadRight(2)} ",
            (if result.Difficulty >= 5 then Color.Red else Color.LightGray),
            Color.FromArgb(0xFF_202020)
        )
        + Console.ColorText(
            $" Next review: {MenuRender.FormatInterval(result.Interval)} ",
            Color.LightGreen,
            Color.FromArgb(0xFF_202020)
        )

    [<Extension>]
    static member LogTo(result: ScheduleResult, this: StudySession) : unit = result.HighlightString() |> this.Log

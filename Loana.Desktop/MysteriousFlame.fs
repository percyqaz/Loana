namespace Loana.Desktop

open System
open System.Text
open System.Drawing
open Loana.Desktop.CLI

type MysteriousFlame() =

    let width = 80
    let p = String.replicate ((Console.BufferWidth - width - 1) / 2) " "
    let s = width * 25
    let b = Array.zeroCreate(s + width + 1)
    let flame_chars = " .:*sS#$"
    let r = Random()
    let bg = Color.FromArgb(0xFF_101010)
    let struct (x, y) = Console.GetCursorPosition()

    member this.Draw() : unit =

        let sb = StringBuilder()

        for i = 0 to 3 do
            b.[int(floor(r.NextDouble() * 60.0)) + 15 + width * 24] <- 80.0

        sb.Append(p.BackColor(bg)) |> ignore

        for i = 0 to s - 1 do
            b.[i] <- floor((b.[i] + b.[i + 1] + b.[i + width] + b.[i + width + 1]) / 4.0)
            let color = Color.FromArgb(255, 255, int(b.[i] * 24.0) |> min 255, 0)

            if i / width < 24 then
                sb.Append(
                    flame_chars.[min 7 (int b.[i])]
                        .ToString()
                        .ForeColor(color)
                        .BackColor(Color.FromArgb(255, 16 + int color.G / 2, 16 + int color.G / 4, 16))
                )
                |> ignore

                if i % width >= width - 1 then
                    sb.Append((p + " \n" + (if i / width < 23 then p else "")).BackColor(bg)) |> ignore

        Console.SetCursorPosition(x, y)
        Console.Write(sb.ToString())

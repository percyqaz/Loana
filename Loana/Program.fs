open System.IO
open System.Drawing
open Loana.CLI
open Loana.Data
open Loana

type Loana =
    static member GetFilePath([<System.Runtime.CompilerServices.CallerFilePath>] ?path: string) =
        System.IO.Path.GetDirectoryName(path.Value)

if System.OperatingSystem.IsWindows() then
    System.Console.CursorVisible <- false
    System.Console.Title <- "Loana v0.1"

System.Console.Clear()
Console.WriteLine(MenuRender.Pad "Loading ...", Color.White, Color.FromArgb(0x303030))

let config = ResizeArray(try File.ReadAllLines("config") with :? FileNotFoundException -> [||])
let data_path =
    while config.Count < 1 || not (Directory.Exists(config.[0])) do
        Console.Write("Enter a path to store data: ")
        let user_input = System.Console.ReadLine()
        if Directory.Exists(user_input) then
            config.Add(user_input)
            File.WriteAllLines("config", [|user_input|])
        else
            Console.WriteLine("That path doesn't exist, put in something else")
    config.[0]

let scheduler = ReviewSchedule(Path.Combine(data_path, "cards.dat"))
let words = WordBank.ReadDirectory(Path.Combine(data_path))

let mysterious_flame =
    let p = "           " in let s = 80 * 25 in let b = Array.zeroCreate (s + 81) in let c = " .:*sS#$"
    let r = System.Random() in let bg = Color.FromArgb(0x101010) in let struct (x, y) = System.Console.GetCursorPosition()
    fun () ->
        MenuRender.WriteLine(MenuRender.Pad "Loana has loaded!", Color.LightGreen, Color.FromArgb(0x303030))
        for i = 0 to 3 do b.[int(floor(r.NextDouble() * 60.0)) + 15 + 80 * 24] <- 80.0
        MenuRender.Write(p, Color.White, bg)
        for i = 0 to s - 1 do
            b.[i] <- floor((b.[i] + b.[i + 1] + b.[i + 80] + b.[i + 81]) / 4.0)
            let color = Color.FromArgb(255, int (b.[i] * 24.0) |> min 255, 0)
            if i / 80 < 24 then
                MenuRender.Write(c[min 7 (int b.[i])].ToString(), color, Color.FromArgb(16 + int color.G / 2, 16 + int color.G / 4, 16))
                if i % 80 > 78 then MenuRender.Write(p + " \n" + (if i / 80 < 23 then p else ""), Color.White, bg)
        MenuRender.WriteLine(MenuRender.Pad "[Any Key] Launch", Color.LightGray, Color.FromArgb(0x202020))
        System.Console.SetCursorPosition(x, y); MenuRender.FlushInline()

while not System.Console.KeyAvailable do
    System.Threading.Thread.Sleep(20)
    mysterious_flame()
System.Console.ReadKey() |> ignore

Menu(words, scheduler).Run()
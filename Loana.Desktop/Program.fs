open System
open System.IO
open System.Drawing
open Loana.Data
open Loana.Desktop
open Loana.Desktop.CLI
open Loana.Desktop.Browser

type Loana =
    static member GetFilePath([<Runtime.CompilerServices.CallerFilePath>] ?path: string) =
        Path.GetDirectoryName(path.Value)

if OperatingSystem.IsWindows() then
    Console.CursorVisible <- false
    Console.Title <- "Loana v0.1"

let config = ResizeArray(try File.ReadAllLines("config") with :? FileNotFoundException -> [||])
let data_path =
    while config.Count < 1 || not (Directory.Exists(config.[0])) do
        Console.Write("Enter a path to store data: ")
        let user_input = Console.ReadLine()
        if Directory.Exists(user_input) then
            config.Add(user_input)
            File.WriteAllLines("config", [|user_input|])
        else
            Console.WriteLine("That path doesn't exist, put in something else")
    config.[0]

let mutable waiting_acceptance = true
while waiting_acceptance do

    Console.Clear()
    Console.WriteLine(MenuRender.Pad "Loading ...", Color.White, Color.FromArgb(0xFF_303030))

    let scheduler = ReviewSchedule(Path.Combine(data_path, "cards.dat"))
    let words = WordBank.FromDirectory(Path.Combine(data_path))
    let verbs = VerbBank(Path.Combine(data_path, "verbs.verblist"))

    let mysterious_flame =
        let p = "           " in let s = 80 * 25 in let b = Array.zeroCreate (s + 81) in let c = " .:*sS#$"
        let r = Random() in let bg = Color.FromArgb(0xFF101010) in let struct (x, y) = Console.GetCursorPosition()
        fun () ->
            MenuRender.WriteLine(MenuRender.Pad "Loana has loaded!", Color.LightGreen, Color.FromArgb(0xFF_303030))
            for i = 0 to 3 do b.[int(floor(r.NextDouble() * 60.0)) + 15 + 80 * 24] <- 80.0
            MenuRender.Write(p, Color.White, bg)
            for i = 0 to s - 1 do
                b.[i] <- floor((b.[i] + b.[i + 1] + b.[i + 80] + b.[i + 81]) / 4.0)
                let color = Color.FromArgb(255, 255, int (b.[i] * 24.0) |> min 255, 0)
                if i / 80 < 24 then
                    MenuRender.Write(c[min 7 (int b.[i])].ToString(), color, Color.FromArgb(255, 16 + int color.G / 2, 16 + int color.G / 4, 16))
                    if i % 80 > 78 then MenuRender.Write(p + " \n" + (if i / 80 < 23 then p else ""), Color.White, bg)
            MenuRender.WriteLine(MenuRender.Pad "[S] Sync  [C] Categorise  [R] Reload  [Enter] Launch", Color.LightGray, Color.FromArgb(0xFF_202020))
            Console.SetCursorPosition(x, y); MenuRender.FlushInline()

    let mutable loop = true
    while loop do
        while not Console.KeyAvailable do
            System.Threading.Thread.Sleep(20)
            mysterious_flame()

        match System.Console.ReadKey(true).Key with
        | ConsoleKey.Enter ->
            loop <- false
            waiting_acceptance <- false
            Menu(words, verbs, scheduler).Run()
        | ConsoleKey.R -> loop <- false
        | ConsoleKey.C ->
            loop <- false
            WordBrowser(words).Run()
        | ConsoleKey.S ->
            loop <- false
            Console.Clear()
            Console.Write("Enter address (blank to host): ")
            let address = Console.ReadLine()
            if address <> "" then Sync.connect(scheduler, words, address)
            else Sync.host(scheduler, words)
            Console.ReadLine() |> ignore
        | ConsoleKey.Escape ->
            loop <- false
            waiting_acceptance <- false
        | _ -> ()
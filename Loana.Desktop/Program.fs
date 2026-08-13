open System
open System.IO
open System.Drawing
open Loana.Data
open Loana.Desktop
open Loana.Desktop.CLI

let config_path =
    Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".loana")

let config =
    ResizeArray(
        try
            File.ReadAllLines(config_path)
        with :? FileNotFoundException ->
            [||]
    )

let data_path =
    while config.Count < 1 || not(Directory.Exists(config.[0])) do
        Console.Write("Enter a path to store data: ")
        let user_input = Console.ReadLine()

        if Directory.Exists(user_input) then
            config.Add(user_input)
            File.WriteAllLines(config_path, [| user_input |])
        else
            Console.WriteLine("That path doesn't exist, put in something else")

    config.[0]

Console.CursorVisible <- false
Console.Clear()

MenuRender.UpdateWidth()

Console.WriteLine(
    MenuRender.Pad($"Loading ({data_path})").ForeColor(Color.White).BackColor(Color.FromArgb(0xFF_303030))
)

let state = LoanaState.Create(data_path)

for error in state.Words.Errors do
    Console.WriteLine(error)

Console.WriteLine(
    MenuRender.Pad("Loana has loaded!").ForeColor(Color.LightGreen).BackColor(Color.FromArgb(0xFF_303030))
)

let mysterious_flame = MysteriousFlame()

while not Console.KeyAvailable do
    System.Threading.Thread.Sleep(20)
    mysterious_flame.Draw()

Console.ReadKey(true) |> ignore

let ctx = UIContext.Create()

MenuView(MenuState.Create(state)).Run(ctx)

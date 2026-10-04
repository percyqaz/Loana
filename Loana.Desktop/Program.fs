open System
open System.IO
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

Console.Clear()

let state = LoanaState.Create(data_path)

for error in state.Words.Errors do
    Console.WriteLine($" {error.WordlistName}:{error.LineNumber} ".BackColor(0x101010) + " " + error.Reason)
    Console.WriteLine(error.Line.ForeColor(0xFF8888))

if state.Words.HasErrors() then
    Console.ReadKey(true) |> ignore

let ctx = UIContext.Create()
MenuView(MenuState.Create(state, ctx)).Run()

state.Scheduler.Save()

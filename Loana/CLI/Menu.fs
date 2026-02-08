namespace Loana.CLI

open System
open System.Drawing
open Loana.CLI

type SelectMenuOption = { Name: string; Action: unit -> unit }

type SelectMenu(options: SelectMenuOption array) =

    let mutable selected = 0

    member private this.Draw() : unit =
        Console.Clear()
        for i = 0 to options.Length - 1 do
            if i = selected then
                Console.Write("> ")
                Console.Write(options.[i].Name, Color.Yellow)
                Console.WriteLine(" <")
            else
                Console.Write(sprintf "  %s" options.[i].Name, Color.LightGray)
                Console.WriteLine()

        Console.WriteLine()

    member this.Show() =
        let mutable loop = true
        while loop do
            this.Draw()
            match Console.ReadKey(true).Key with
            | ConsoleKey.Escape -> loop <- false
            | ConsoleKey.Enter -> options.[selected].Action()
            | ConsoleKey.UpArrow -> selected <- (selected + options.Length - 1) % options.Length
            | ConsoleKey.DownArrow -> selected <- (selected + 1) % options.Length
            | _ -> ()
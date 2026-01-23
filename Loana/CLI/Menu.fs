namespace Loana.CLI

open System
open System.Drawing
open Loana.CLI

type SelectMenuOption = { Name: string; Action: unit -> unit }

// todo: just label each thing and then you type what you want
type SelectMenu(options: SelectMenuOption array) =

    let mutable selected = 0

    member private this.Draw() : unit =
        Console.Clear()
        for i = 0 to options.Length - 1 do
            if i = selected then
                Console.Write(" > ")
                Console.Write(options.[i].Name, Color.Yellow)
                Console.WriteLine(" <")
            else
                let offset = (options.Length + i - selected) % options.Length
                Console.Write(sprintf "%02i %s" offset options.[i].Name, Color.LightGray)
                Console.WriteLine()

        Console.WriteLine()

    member this.Show() =
        this.Draw()
        let mutable show = true
        while show do
            let user_input = Console.ReadLine()
            match user_input with
            | "back" -> show <- false
            | ""
            | "ok" ->
                options.[selected].Action()
                this.Draw()
            | _ ->
                match Int32.TryParse(user_input) with
                | true, n ->
                    selected <- ((selected + n) % options.Length + options.Length) % options.Length
                | false, _ -> ()
                this.Draw()
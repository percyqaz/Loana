namespace Loana.Verbs

open System
open System.IO
open System.Drawing
open Loana.CLI
open Loana.Language

type Verbs =

    static member AddVerb(input: string) : unit =
        failwith "nyi"

    static member AddVerbs() =
        let mutable loop = true
        while loop do
            let input = Console.ReadLine()
            if input = "" then loop <- false else
                try Verbs.AddVerb(input) with err -> Console.WriteLine(err.Message + "\n\n" + err.StackTrace, Color.Red)
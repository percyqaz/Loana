namespace Loana.Features

open System
open System.IO
open System.Drawing
open Loana.CLI
open Loana.Language

type Verbs =

    static member AddVerb(input: string) : unit =
        let verb = VerbDownloader.extend_verb { Infinitive = Vocab.Parse input; PastParticiple = Nothing; Inflections = [] }
        let lines =
            seq {
                yield verb.Infinitive.ToString()
                for inflection in verb.Inflections do
                    yield " " + inflection.ToString()
            }
        File.AppendAllLines("C:/Users/percy/Desktop/Source/Loana/Wordlists/core-verbs.wordlist", lines)

    static member AddVerbs() =
        let mutable loop = true
        while loop do
            let input = Console.ReadLine()
            if input = "" then loop <- false else
                try Verbs.AddVerb(input) with err -> Console.WriteLine(err.Message + "\n\n" + err.StackTrace, Color.Red)
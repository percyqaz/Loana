namespace Loana.Data

open System
open System.Drawing
open System.IO
open System.Collections.Generic
open Loana.Language

type VerbFile(path: string) =

    member this.Load() =
        let entries = Dictionary<string, Map<VerbInflection, string>>()
        let mutable current_verb: string option = None
        let mutable current_inflections = Map.empty
        try
            File.ReadAllLines(path)
        with
        | :? FileNotFoundException ->
            [||]
        |> Seq.where (fun line -> line.Trim() <> "")
        |> Seq.iter (fun line ->
            if line.[0] = '|' then
                let split = line.Substring(2).Split(" = ", StringSplitOptions.TrimEntries)
                current_inflections <- current_inflections.Add(VerbInflection.Parse(split.[0]), split.[1])
            else
                match current_verb with
                | None -> if not current_inflections.IsEmpty then failwith "Inflections above first verb!"
                | Some v -> entries.Add(v, current_inflections)
                current_verb <- Some line
        )
        match current_verb with
        | None -> if not current_inflections.IsEmpty then failwith "Inflections above first verb!"
        | Some v -> entries.Add(v, current_inflections)

        entries

    member this.Write(data: Dictionary<string, Map<VerbInflection, string>>) =
        let bak_path = path + ".bak"
        let temp_path = path + ".tmp"

        let lines = seq {
            for key in data.Keys do
                yield key
                let map = data.[key]
                for inflection in map do
                    yield sprintf "| %O = %s" inflection.Key inflection.Value
        }
        File.WriteAllLines(temp_path, lines)

        try File.Delete(bak_path) with _ -> ()
        try File.Move(path, bak_path) with :? FileNotFoundException -> ()
        File.Move(temp_path, path)

type VerbBank(path: string) =

    let db = VerbFile(path)
    let mem = db.Load()

    member this.Get(verb: Verb) : Map<VerbInflection, string> voption =
        match mem.TryGetValue(verb.Infinitive.Deutsch) with
        | true, data -> ValueSome data
        | false, _ -> ValueNone

    member this.Update(verb: Verb, inflections: Map<VerbInflection, string>) : unit =
        mem.[verb.Infinitive.Deutsch] <- inflections
        db.Write(mem)

    member this.Ensure(verb: Verb) : Map<VerbInflection, string> =
        let mutable inflections = this.Get(verb) |> ValueOption.defaultValue Map.empty
        let mutable missing = false
        for q in verb.Quizzes do
            match q with
            | VerbQuiz.Present ->
                if
                    not (inflections.ContainsKey(VerbInflection.Present TensePerson.ThirdSingular)
                    || inflections.ContainsKey(VerbInflection.Present TensePerson.FirstSingular))
                then
                    Console.WriteLine(sprintf "'%s' is missing present inflections" verb.Infinitive.Deutsch, Color.Yellow)
                    missing <- true
            | VerbQuiz.SimplePast ->
                if
                    not (inflections.ContainsKey(VerbInflection.SimplePast TensePerson.ThirdSingular)
                    || inflections.ContainsKey(VerbInflection.SimplePast TensePerson.FirstSingular))
                then
                    Console.WriteLine(sprintf "'%s' is missing present inflections" verb.Infinitive.Deutsch, Color.Yellow)
                    missing <- true
            | VerbQuiz.Imperative ->
                if
                    not (inflections.ContainsKey(VerbInflection.Imperative ImperativePerson.SecondSingular))
                then
                    Console.WriteLine(sprintf "'%s' is missing present inflections" verb.Infinitive.Deutsch, Color.Yellow)
                    missing <- true

        if missing then
            for key, value in VerbDownloader.fetch_verb_inflections(verb) do
                inflections <- inflections.Add(key, value)
            this.Update(verb, inflections)
            Console.WriteLine(sprintf "Added missing inflections for '%O'" verb)
        inflections
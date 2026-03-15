namespace Loana.Data

open System
open System.IO
open System.Collections.Generic
open Loana.Language
open Loana.Verbs

type VerbFile(path: string) =

    member this.Load() =
        let entries = Dictionary<string, Map<VerbInflection, string>>()
        let mutable current_verb: string option = None
        let mutable current_inflections = Map.empty
        File.ReadAllLines(path)
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
        File.Move(path, bak_path)
        File.Move(temp_path, path)
        
type VerbBank(path: string) =

    let db = VerbFile(path)
    let mem = db.Load()

    member this.Get(verb: Verb) : Map<VerbInflection, string> voption =
        match mem.TryGetValue(verb.Infinitive.Deutsch) with
        | true, data -> ValueSome data
        | false, _ -> ValueNone

    member this.Update(verb: Verb, inflections: Map<VerbInflection, string>) =
        mem.[verb.Infinitive.Deutsch] <- inflections
        db.Write(mem)
    
    member this.Ensure(verb: Verb) =
        let mutable inflections = this.Get(verb) |> ValueOption.defaultValue Map.empty
        for key, value in VerbDownloader.fetch_verb_inflections(verb) do
            inflections <- inflections.Add(key, value)
        this.Update(verb, inflections)
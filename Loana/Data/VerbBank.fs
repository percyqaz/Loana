namespace Loana.Data

open System
open System.Drawing
open System.IO
open System.Collections.Generic
open Loana.Language

type VerbFile(path: string) =

    member this.Load() : Dictionary<_, _> =
        let entries = Dictionary<string, Map<VerbInflection, string>>()

        let read_all_lines () : string seq =
            try
                File.ReadAllLines(path) |> Seq.where(fun line -> line.Trim() <> "")
            with :? FileNotFoundException ->
                [||]

        let mutable current_verb: string option = None
        let mutable current_inflections = Map.empty

        let finish_current_verb () =
            match current_verb with
            | None ->
                if not current_inflections.IsEmpty then
                    failwith "Inflections above first verb!"
            | Some v -> entries.Add(v, current_inflections)

        let handle_line (line: string) =
            let is_inflection = line.[0] = '|'

            if is_inflection then
                let split_by_equals = line.Substring(2).Split(" = ", StringSplitOptions.TrimEntries)

                current_inflections <-
                    current_inflections.Add(VerbInflection.FromString(split_by_equals.[0]), split_by_equals.[1])
            else
                finish_current_verb()
                current_verb <- Some line

        read_all_lines() |> Seq.iter handle_line
        finish_current_verb()
        entries

    member this.Write(verbs: Dictionary<string, Map<VerbInflection, string>>) : unit =
        let lines =
            seq {
                for verb in verbs.Keys do
                    yield verb
                    let inflections = verbs.[verb]

                    for inflection in inflections do
                        yield sprintf "| %O = %s" inflection.Key inflection.Value
            }

        let bak_path = path + ".bak"
        let temp_path = path + ".tmp"

        File.WriteAllLines(temp_path, lines)

        try
            File.Delete(bak_path)
        with _ ->
            ()

        try
            File.Move(path, bak_path)
        with :? FileNotFoundException ->
            ()

        File.Move(temp_path, path)

type VerbBank(path: string) =

    let db = VerbFile(path)
    let verbs = db.Load()

    member this.Get(verb: Verb) : Map<VerbInflection, string> voption =
        match verbs.TryGetValue(verb.Infinitive.Deutsch) with
        | true, data -> ValueSome data
        | false, _ -> ValueNone

    member this.Update(verb: Verb, inflections: Map<VerbInflection, string>) : unit =
        verbs.[verb.Infinitive.Deutsch] <- inflections
        db.Write(verbs)

    member this.EnsureAllInflectionsAvailable(verb: Verb) : Map<VerbInflection, string> =
        let mutable inflections = this.Get(verb) |> ValueOption.defaultValue Map.empty

        let missing_present_inflections () =
            not(inflections.ContainsKey(VerbInflection.Present(InflectionPerson.ThirdSingular)))
            && not(inflections.ContainsKey(VerbInflection.Present(InflectionPerson.FirstSingular)))

        let missing_past_inflections () =
            not(inflections.ContainsKey(VerbInflection.SimplePast(InflectionPerson.ThirdSingular)))
            && not(inflections.ContainsKey(VerbInflection.SimplePast(InflectionPerson.FirstSingular)))

        let missing_imperative_inflections () =
            not(inflections.ContainsKey(VerbInflection.Imperative(ImperativePerson.SecondSingular)))

        let detect_missing () : bool =
            let mutable has_missing = false

            for requested_tense in verb.Tenses do
                match requested_tense with
                | VerbTense.Present when missing_present_inflections() ->
                    Console.WriteLine(
                        sprintf "'%s' is missing present inflections" verb.Infinitive.Deutsch,
                        Color.Yellow
                    )

                    has_missing <- true
                | VerbTense.SimplePast when missing_past_inflections() ->
                    Console.WriteLine(sprintf "'%s' is missing past inflections" verb.Infinitive.Deutsch, Color.Yellow)
                    has_missing <- true
                | VerbTense.Imperative when missing_imperative_inflections() ->
                    Console.WriteLine(
                        sprintf "'%s' is missing imperative inflections" verb.Infinitive.Deutsch,
                        Color.Yellow
                    )

                    has_missing <- true
                | _ -> ()

            has_missing

        let download_inflections () =
            for inflection, inflected_text in VerbDownloader.fetch_verb_inflections(verb) do
                inflections <- inflections.Add(inflection, inflected_text)

            this.Update(verb, inflections)
            Console.WriteLine(sprintf "Added missing inflections for '%O'" verb)

        if detect_missing() then
            download_inflections()

        inflections

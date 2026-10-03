namespace Loana.Data

open System
open System.Drawing
open System.IO
open System.Collections.Generic
open Loana.Language

type VerbFile(path: string) =

    member this.Load() : Dictionary<_, _> =
        let entries = Dictionary<string, Map<Conjugation, string>>()

        let read_all_lines () : string seq =
            try
                File.ReadAllLines(path) |> Seq.where(fun line -> line.Trim() <> "")
            with :? FileNotFoundException ->
                [||]

        let mutable current_verb: string option = None
        let mutable current_conjugations = Map.empty

        let finish_current_verb () =
            match current_verb with
            | None ->
                if not current_conjugations.IsEmpty then
                    failwith "Conjugations above first verb!"
            | Some v ->
                entries.Add(v, current_conjugations)
                current_conjugations <- Map.empty

        let handle_line (line: string) =
            let is_conjugation = line.[0] = '|'

            if is_conjugation then
                let split_by_equals = line.Substring(2).Split(" = ", StringSplitOptions.TrimEntries)

                current_conjugations <-
                    current_conjugations.Add(Conjugation.FromString(split_by_equals.[0]), split_by_equals.[1])
            else
                finish_current_verb()
                current_verb <- Some line

        read_all_lines() |> Seq.iter handle_line
        finish_current_verb()
        entries

    member this.Write(verbs: Dictionary<string, Map<Conjugation, string>>) : unit =
        let lines =
            seq {
                for verb in verbs.Keys do
                    yield verb
                    let conjugations = verbs.[verb]

                    for conjugation in conjugations do
                        yield sprintf "| %O = %s" conjugation.Key conjugation.Value
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

    member this.Get(verb: Verb) : Map<Conjugation, string> voption =
        match verbs.TryGetValue(verb.Infinitive.Deutsch) with
        | true, data -> ValueSome data
        | false, _ -> ValueNone

    member this.Update(verb: Verb, conjugations: Map<Conjugation, string>) : unit =
        verbs.[verb.Infinitive.Deutsch] <- conjugations
        db.Write(verbs)

    member this.EnsureAllConjugationsAvailable(verb: Verb) : Map<Conjugation, string> =
        let mutable conjugations = this.Get(verb) |> ValueOption.defaultValue Map.empty

        let missing_present () =
            not(conjugations.ContainsKey(Conjugation.Present(ConjugationPerson.ThirdSingular)))
            && not(conjugations.ContainsKey(Conjugation.Present(ConjugationPerson.FirstSingular)))

        let missing_past () =
            not(conjugations.ContainsKey(Conjugation.SimplePast(ConjugationPerson.ThirdSingular)))
            && not(conjugations.ContainsKey(Conjugation.SimplePast(ConjugationPerson.FirstSingular)))

        let missing_imperative () =
            not(conjugations.ContainsKey(Conjugation.Imperative(ImperativePerson.SecondSingular)))

        let detect_missing () : bool =
            let mutable has_missing = false

            for requested_tense in verb.Tenses do
                match requested_tense with
                | VerbTense.Present when missing_present() ->
                    Console.WriteLine(
                        sprintf "'%s' is missing present conjugations" verb.Infinitive.Deutsch,
                        Color.Yellow
                    )

                    has_missing <- true
                | VerbTense.SimplePast when missing_past() ->
                    Console.WriteLine(sprintf "'%s' is missing past conjugations" verb.Infinitive.Deutsch, Color.Yellow)
                    has_missing <- true
                | VerbTense.Imperative when missing_imperative() ->
                    Console.WriteLine(
                        sprintf "'%s' is missing imperative conjugations" verb.Infinitive.Deutsch,
                        Color.Yellow
                    )

                    has_missing <- true
                | _ -> ()

            has_missing

        let download_missing () =
            let parsed = ParsedVerb.FromVerb(verb)
            let ctx = ConjugationDownloader.fetch_conjugations(parsed)

            for conjugation, conjugated_text in ctx.ConjugateAll(verb) do
                conjugations <- conjugations.Add(conjugation, conjugated_text)

            this.Update(verb, conjugations)
            Console.WriteLine(sprintf "Added missing conjugations for '%O'" verb)

        if detect_missing() then
            download_missing()

        conjugations

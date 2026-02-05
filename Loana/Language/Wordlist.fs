namespace Loana.Language

open System
open System.Drawing
open Loana.CLI

module Wordlist =

    let internal parse_noun_inner(vocab: Vocab, tags: string list) : Noun =
        let mutable mtags = tags
        let mutable gender : Gender option = None
        let mutable plural : Vocab option = None
        let mutable noplural : bool = false

        while mtags <> [] do
            let next = mtags.Head
            mtags <- mtags.Tail
            match next with
            | "p" | "m" | "f" | "n" ->
                if gender.IsSome then failwithf "Gender was set twice for noun: %O" vocab
                gender <- Some (Gender.Parse next)
            | "no_plural" ->
                if gender.IsNone then failwithf "'no_plural' must be set after gender for noun: %O" vocab
                noplural <- true
            | "plural" ->
                if gender.IsNone then failwithf "plural must be set after gender for noun: %O" vocab
                if noplural then failwithf "plural conflicts with 'no_plural' for noun: %O" vocab
                plural <- Some (Vocab.Parse (String.concat " " mtags))
                mtags <- []
            | _ -> failwithf "Unrecognised tag '%s' for noun: %O" next vocab

        let guts_plural = if noplural then Nothing else match plural with Some p -> Something p | None -> ToBeDetermined
        {
            Translation = vocab
            Guts =
                match gender with
                | None -> failwithf "No gender was specified for this noun! Got: %O :%s" vocab (String.concat " " tags)
                | Some Gender.Masculine -> Masculine guts_plural
                | Some Gender.Feminine -> Feminine guts_plural
                | Some Gender.Neuter -> Neuter guts_plural
                | Some Gender.Plural -> Plural
        }

    let internal parse_core (line: string) : Vocab * string list =
        if line = "" then failwith "Cannot parse empty line as a noun"

        let split = line.Split(":", 2, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        let tags =
            if split.Length = 2 then
                split.[1].Split(" ", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
            else
                []
        let vocab = Vocab.Parse split.[0]
        vocab, tags

    let parse_noun : string -> Noun =
        parse_core >> parse_noun_inner

    let parse_vocab : string -> Vocab =
        parse_core >> fst

type WordlistItem =
    | Noun of Noun
    | Verb of Verb
    | Vocab of Vocab

type WordlistEntry =
    {
        Source: string
        Item: WordlistItem
    }

open System.IO

type Wordlist() =

    let sources = ResizeArray<string>()
    let entries = ResizeArray<WordlistEntry>()

    let deduplicate_de = Collections.Generic.Dictionary<string, Vocab * string>()
    let deduplicate_en = Collections.Generic.Dictionary<string, Vocab * string>()

    let mutable current_verb = None

    let finish_verb(source: string) =
        match current_verb with
        | Some v -> entries.Add { Item = Verb v; Source = source }; current_verb <- None
        | None -> ()

    let check_duplicate (source: string) (v: Vocab) : unit =

        let ded_de = v.Key
        if deduplicate_de.ContainsKey(ded_de) then
            let duplicate_of, d_source = deduplicate_de.[ded_de]
            failwithf "'%O' conflicts with '%O' in '%s'" v duplicate_of d_source
        else
            deduplicate_de.Add(ded_de, (v, source))

        let ded_en = v.EnglishKey
        if deduplicate_en.ContainsKey(ded_en) then
            let duplicate_of, d_source = deduplicate_en.[ded_en]
            failwithf "'%O' conflicts with '%O' in '%s'" v duplicate_of d_source
        else
            deduplicate_en.Add(ded_en, (v, source))

    let add_vocab (source: string) (line: string) : unit =
        finish_verb source
        let v, tags = Wordlist.parse_core line

        check_duplicate source v

        if v.DetectVerb then
            current_verb <- Some { Infinitive = v; Inflections = [] }
        elif v.DetectNoun && tags <> [] then
            entries.Add { Item = Wordlist.parse_noun_inner(v, tags) |> Noun; Source = source }
        else
            entries.Add { Item = Vocab v; Source = source }

    let add_inflection (source: string) (line: string) : unit =
        let v, _ = Wordlist.parse_core line

        check_duplicate source v

        match current_verb with
        | Some verb ->
            current_verb <- Some (verb.WithInflection(v))
        | None -> failwithf "Verb inflection not attached to a verb: %s" line

    let add_dynamic (source: string) (line: string) : unit =
        if line = "" || line.StartsWith "#" then
            () // reserved for comments for now
        elif line.[0] = ' ' then
            add_inflection source line
        else
            add_vocab source line

    member this.TryAdd(source: string, line: string) : Result<unit, string> =
        try add_dynamic source line; Ok()
        with err -> Error err.Message

    member this.ReadFile(path: string) =
        let filename = Path.GetFileNameWithoutExtension(path)
        sources.Add(filename)
        let mutable count = 0
        File.ReadAllLines(path)
        |> Seq.where (fun line -> line.Trim() <> "")
        |> Seq.iter (fun line ->
            match this.TryAdd(filename, line) with
            | Ok() -> count <- count + 1
            | Error reason ->
                Console.Write($" {filename}: ", Color.LightBlue, Color.FromArgb 0x202020)
                Console.WriteLine(" " + reason, Color.Red)
        )
        finish_verb filename
        Console.WriteLine(sprintf "Successfully read %i entries from '%s'" count filename, Color.LightGreen)

    member this.ReadDirectory(path: string) =
        let meta_list = Path.Combine(path, "wordlists.meta")
        Console.WriteLine(sprintf "Reading wordlist meta from '%s'" meta_list)
        let lines =
            try File.ReadAllLines(meta_list)
            with err -> Console.WriteLine(err.Message, Color.Red); [||]
        for source in lines do
            let path = Path.Combine(path, source + ".wordlist")
            if Path.Exists(path) then
                this.ReadFile(path)
            else
                Console.WriteLine(sprintf "Could not find wordlist '%s' at %s" source path, Color.Red)

    member this.Entries = entries.AsReadOnly()
    member this.Sources = sources.AsReadOnly()

    member this.Stats() =
        Console.WriteLine(sprintf " %i Entries " this.Entries.Count, Color.LightGreen, Color.FromArgb(0x202020))
        let nouns = this.Entries |> Seq.choose (fun e -> match e.Item with Noun n -> Some n | _ -> None) |> Array.ofSeq
        let missing_plural = nouns |> Seq.where (fun n -> match n.Guts with Plural -> false | Masculine x | Feminine x | Neuter x -> x.IsToBeDetermined) |> Array.ofSeq
        Console.WriteLine(sprintf " %i Nouns + Gender " nouns.Length, Color.White, Color.FromArgb(0x202020))
        for gender, count in nouns |> Seq.countBy _.Guts.Gender do
            Console.Write($"[{gender}]", gender.Color)
            Console.WriteLine(sprintf ": %i" count)
        Console.WriteLine(sprintf "%i nouns are missing plurals" missing_plural.Length, Color.Yellow)

        let verbs = this.Entries |> Seq.choose (fun e -> match e.Item with Verb v -> Some v | _ -> None) |> Array.ofSeq
        let inflections = verbs |> Seq.map (fun v -> v.Inflections.Length) |> Seq.sum
        Console.WriteLine(sprintf " %i Verbs " verbs.Length, Color.White, Color.FromArgb(0x202020))
        Console.WriteLine(sprintf "+ %i inflections" inflections)

        let other = this.Entries |> Seq.choose (fun e -> match e.Item with Vocab v -> Some v | _ -> None) |> Array.ofSeq
        let could_be_nouns = other |> Seq.where(fun v -> v.DetectNoun)
        Console.WriteLine(sprintf " %i Uncategorised " other.Length, Color.White, Color.FromArgb(0x202020))
        Console.WriteLine(sprintf "%i potential nouns missing a gender" (Seq.length could_be_nouns), Color.Yellow)

        Console.ReadLine() |> ignore
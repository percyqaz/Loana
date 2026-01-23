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

    let entries = ResizeArray<WordlistEntry>()

    let deduplicate_de = Collections.Generic.Dictionary<string, Vocab>()
    let deduplicate_en = Collections.Generic.Dictionary<string, Vocab>()

    let add_dynamic (source: string) (line: string) : unit =
        let v, tags = Wordlist.parse_core line

        let ded_de = v.Key
        if deduplicate_de.ContainsKey(ded_de) then
            failwithf "'%O' conflicts (German) with '%O'" v deduplicate_de.[ded_de]
        else
            deduplicate_de.Add(ded_de, v)

        let ded_en = v.EnglishKey
        if deduplicate_en.ContainsKey(ded_en) then
            failwithf "'%O' conflicts (English) with '%O'" v deduplicate_en.[ded_en]
        else
            deduplicate_en.Add(ded_en, v)

        let item =
            if Char.IsUpper v.Deutsch.[0] && tags <> [] then
                Wordlist.parse_noun_inner(v, tags) |> Noun
            else
                Vocab v

        entries.Add { Item = item; Source = source }

    member this.TryAdd(source: string, line: string) : Result<unit, string> =
        try add_dynamic source line; Ok()
        with err -> Error err.Message

    member this.ReadFile(path: string) =
        let filename = Path.GetFileNameWithoutExtension(path)
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
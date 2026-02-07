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

[<Struct>]
type Source = { Group: string; File: string }

type WordlistEntry =
    {
        Source: Source
        Item: WordlistItem
    }

open System.IO

type WordlistGroup = { Name: string; Lists: ResizeArray<string> }

type WordBank() =

    let groups = ResizeArray<WordlistGroup>()
    let entries = ResizeArray<WordlistEntry>()

    let deduplicate_de = Collections.Generic.Dictionary<string, Vocab * string>()
    let deduplicate_en = Collections.Generic.Dictionary<string, Vocab * string>()

    let mutable current_verb = None

    let finish_verb(source: Source) =
        match current_verb with
        | Some v -> entries.Add { Item = Verb v; Source = source }; current_verb <- None
        | None -> ()

    let check_duplicate (source: Source) (v: Vocab) : unit =

        let ded_de = v.Key
        if deduplicate_de.ContainsKey(ded_de) then
            let duplicate_of, d_source = deduplicate_de.[ded_de]
            failwithf "'%O' conflicts with '%O' in '%s'" v duplicate_of d_source
        else
            deduplicate_de.Add(ded_de, (v, source.File))

        let ded_en = v.EnglishKey
        if deduplicate_en.ContainsKey(ded_en) then
            let duplicate_of, d_source = deduplicate_en.[ded_en]
            failwithf "'%O' conflicts with '%O' in '%s'" v duplicate_of d_source
        else
            deduplicate_en.Add(ded_en, (v, source.File))

    let add_vocab (source: Source) (line: string) : unit =
        finish_verb source
        let v, tags = Wordlist.parse_core line

        check_duplicate source v

        if v.DetectVerb then
            // todo: support :pp deutsch = english notation
            current_verb <- Some { Infinitive = v; PastParticiple = Nothing; Inflections = [] }
        elif v.DetectNoun && tags <> [] then
            entries.Add { Item = Wordlist.parse_noun_inner(v, tags) |> Noun; Source = source }
        else
            entries.Add { Item = Vocab v; Source = source }

    let add_inflection (source: Source) (line: string) : unit =
        let v, _ = Wordlist.parse_core line

        check_duplicate source v

        match current_verb with
        | Some verb ->
            current_verb <- Some (verb.WithInflection(v))
        | None -> failwithf "Verb inflection not attached to a verb: %s" line

    let add_dynamic (source: Source) (line: string) : unit =
        if line = "" || line.StartsWith "#" then
            () // reserved for comments for now
        elif line.[0] = ' ' then
            add_inflection source line
        else
            add_vocab source line

    member this.TryAdd(source: Source, line: string) : Result<unit, string> =
        try add_dynamic source line; Ok()
        with err -> Error err.Message

    member this.ReadFile(source: Source, path: string) =
        File.ReadAllLines(path)
        |> Seq.where (fun line -> line.Trim() <> "")
        |> Seq.iter (fun line ->
            match this.TryAdd(source, line) with
            | Ok() -> ()
            | Error reason ->
                Console.Write($" {source.File}: ", Color.LightBlue, Color.FromArgb 0x202020)
                Console.WriteLine(" " + reason, Color.Red)
        )
        finish_verb source

    member this.ReadDirectory(path: string) =
        let meta_list = Path.Combine(path, "wordlists.meta")
        if File.Exists(meta_list) |> not then
            Console.WriteLine(sprintf "'%s' doesn't exist!" meta_list, Color.Red)
        else
            Console.WriteLine(sprintf "Reading wordlist meta from '%s'" meta_list)
            let mutable group : WordlistGroup option = None
            for line in File.ReadAllLines(meta_list) do
                if line.StartsWith("#") then
                    let new_group = { Name = line.TrimStart('#').Trim(); Lists = ResizeArray() }
                    groups.Add new_group
                    group <- Some new_group

                elif group.IsSome then
                    let filename = line.Trim()
                    let wl_path = Path.Combine(path, filename + ".wordlist")
                    if Path.Exists(wl_path) then
                        group.Value.Lists.Add(filename)
                        this.ReadFile({ Group = group.Value.Name; File = filename }, wl_path)
                    else
                        Console.WriteLine(sprintf "Could not find wordlist '%s' at %s" filename wl_path, Color.Red)

                else
                    let filename = line.Trim()
                    Console.WriteLine(sprintf "Wordlist '%s' is not part of a group" filename, Color.Red)

    static member ReadDirectory(path: string) : WordBank =
        let words = WordBank()
        words.ReadDirectory(path)
        words

    member this.Entries = entries.AsReadOnly()
    member this.Groups = groups.AsReadOnly()

    member this.Stats() =
        Console.WriteLine(sprintf " %i Entries " this.Entries.Count, Color.LightGreen, Color.FromArgb(0x202020))
        Console.ReadLine() |> ignore
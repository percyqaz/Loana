namespace Loana.Data

open System
open System.Collections.Generic
open System.Drawing
open System.IO
open Loana.Language

module Wordlist =

    let internal parse_noun_inner(vocab: Vocab, tags: string list) : Noun =
        let mutable remaining_tags = tags
        let mutable gender : Gender option = None
        let mutable plural : Vocab option = None
        let mutable no_plural : bool = false

        while remaining_tags <> [] do
            let next = remaining_tags.Head
            remaining_tags <- remaining_tags.Tail
            match next with
            | "p" | "m" | "f" | "n" ->
                if gender.IsSome then failwithf "Gender was set twice for noun: %O" vocab
                gender <- Some (Gender.FromString next)
            | "no_plural" ->
                if gender.IsNone then failwithf "'no_plural' must be set after gender for noun: %O" vocab
                no_plural <- true
            | "plural" ->
                if gender.IsNone then failwithf "plural must be set after gender for noun: %O" vocab
                if no_plural then failwithf "plural conflicts with 'no_plural' for noun: %O" vocab
                plural <- Some (Vocab.FromString (String.concat " " remaining_tags))
                remaining_tags <- []
            | _ -> failwithf "Unrecognised tag '%s' for noun: %O" next vocab

        let guts_plural =
            if no_plural then KnownNothing
            else
                match plural with
                | Some p -> KnownValue p
                | None -> Unknown
        
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

    let internal parse_verb_inner(vocab: Vocab, tags: string list) : Verb =
        let mutable remaining_tags = tags
        let mutable verb_tenses: VerbTense list = []
        let mutable past_participle: Knowledge<Vocab> = if tags <> [] then KnownNothing else Unknown
        let mutable is_dative = false

        while remaining_tags <> [] do
            let next = remaining_tags.Head
            remaining_tags <- remaining_tags.Tail
            match next with
            | "pa" | "pr" | "im" ->
                verb_tenses <- verb_tenses @ [ VerbTense.FromString(next)]
            | "dat" ->
                if is_dative then failwith "Dative specified twice"
                if verb_tenses <> [] then failwith "Dative must be specified before quizzes"
                is_dative <- true
            | "pp" ->
                past_participle <- KnownValue (Vocab.FromString (String.concat " " remaining_tags))
                remaining_tags <- []
            | _ -> failwithf "Unrecognised tag '%s' for noun: %O" next vocab

        {
            Infinitive = vocab
            PastParticiple = past_participle
            Tenses = verb_tenses |> List.distinct
            Dative = is_dative
        }

    let internal parse_core (line: string) : Vocab * string list =
        if line = "" then failwith "Cannot parse empty line as a noun"

        let split_by_colon = line.Split(":", 2, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        let tags =
            if split_by_colon.Length = 2 then
                split_by_colon.[1].Split(" ", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
            else
                []
        let vocab = Vocab.FromString split_by_colon.[0]
        vocab, tags

    let parse_noun : string -> Noun =
        parse_core >> parse_noun_inner

    let parse_vocab : string -> Vocab =
        parse_core >> fst

type WordlistItem =
    | Noun of Noun
    | Verb of Verb
    | Vocab of Vocab
    override this.ToString() : string =
        match this with
        | Noun n -> n.ToString()
        | Verb v -> v.ToString()
        | Vocab v -> v.ToString()

[<Struct>]
type Source = { Group: string; File: string }

type WordlistEntry =
    {
        Source: Source
        Item: WordlistItem
    }

type WordlistGroup = { Name: string; Lists: ResizeArray<string> }
type DuplicateEntry = { SourceFile: string; Line: int; Item: WordlistItem }

type WordBank(path: string) =

    let groups = ResizeArray<WordlistGroup>()
    let entries = ResizeArray<WordlistEntry>()

    let deduplicate_de = Dictionary<string, DuplicateEntry>()
    let deduplicate_en = Dictionary<string, DuplicateEntry>()

    member private this.CheckDuplicate(source: Source, line_n: int, vocab: Vocab, item: WordlistItem) : unit =
    
        let report_duplicate(language: string, existing_conflict: DuplicateEntry) : unit =
            let is_identical = item = existing_conflict.Item
            if existing_conflict.SourceFile <> source.File || existing_conflict.Line <> line_n then
                failwithf "%s %s definition! \n %s (%s:%i)\n %s (%s:%i)"
                    (if is_identical then "Duplicate" else "Conflict with")
                    language
                    (item.ToString()) source.File line_n
                    (existing_conflict.Item.ToString()) existing_conflict.SourceFile existing_conflict.Line
                    
        let check_language_duplicate(language: string, already_seen: Dictionary<string, _>, ascii_identifier: string) =
            match already_seen.TryGetValue(ascii_identifier) with
            | true, existing_conflict -> report_duplicate(language, existing_conflict)
            | false, _ -> already_seen.Add(ascii_identifier, { SourceFile = source.File; Line = line_n; Item = item })
            
        let de_ascii_identifier = vocab.DeutschAsciiIdentifier
        check_language_duplicate("German", deduplicate_de, de_ascii_identifier)

        let en_ascii_identifier = vocab.EnglishAsciiIdentifier
        check_language_duplicate("English", deduplicate_en, en_ascii_identifier)

    member private this.AddVocab(source: Source, line_n: int, line: string) : unit =
        let v, tags = Wordlist.parse_core line

        if v.LooksLikeAVerb then
            let verb = Wordlist.parse_verb_inner(v, tags)
            match verb.PastParticiple with
            | KnownValue pp -> this.CheckDuplicate(source, line_n, pp, Verb verb)
            | _ -> ()
            this.CheckDuplicate(source, line_n, v, Verb verb)
            entries.Add { Item = Verb verb; Source = source }

        elif v.LooksLikeANoun && tags <> [] then
            let noun = Wordlist.parse_noun_inner(v, tags)
            match noun.Plural with
            | KnownValue plural -> this.CheckDuplicate(source, line_n, plural, Noun noun)
            | _ -> ()
            this.CheckDuplicate(source, line_n, v, Noun noun)
            entries.Add { Item = Noun noun; Source = source }

        else
            this.CheckDuplicate(source, line_n, v, Vocab v)
            entries.Add { Item = Vocab v; Source = source }

    member private this.TryAddLine(source: Source, line_n: int, line: string) : Result<unit, string> =
        try
            let is_comment = line.StartsWith "#"
            if line <> "" && not is_comment then
                this.AddVocab(source, line_n, line)
            Ok()
        with err -> Error err.Message

    member this.ReadFile(source: Source, file_path: string) : unit =
        File.ReadAllLines(file_path)
        |> Seq.iteri (fun line_n line ->
            match this.TryAddLine(source, line_n, line) with
            | Ok() -> ()
            | Error "" -> ()
            | Error reason ->
                Console.Write($" {source.File}: ", Color.LightBlue, Color.FromArgb 0x202020)
                Console.WriteLine(" " + reason, Color.Red)
        )
        
    member private this.GetMetaList() : string array =
        let meta_list = Path.Combine(path, "wordlists.meta")
        if File.Exists(meta_list) |> not then
            Console.WriteLine(sprintf "'%s' doesn't exist!" meta_list, Color.Red)
            [||]
        else
            File.ReadAllLines(meta_list)
            
    member private this.TryLoadWordlist(group: WordlistGroup, wordlist_name: string) =
        let wordlist_path = Path.Combine(path, wordlist_name + ".wordlist")
        if Path.Exists(wordlist_path) then
            group.Lists.Add(wordlist_name)
            this.ReadFile({ Group = group.Name; File = wordlist_name }, wordlist_path)
        else
            Console.WriteLine(sprintf "Could not find wordlist '%s' at %s" wordlist_name wordlist_path, Color.Red)
            
    member private this.Reload() : unit =
        groups.Clear()
        entries.Clear()
        deduplicate_de.Clear()
        deduplicate_en.Clear()

        let mutable current_group : WordlistGroup option = None
        
        let start_new_group(name: string) : unit =
            let new_group = { Name = name; Lists = ResizeArray() }
            groups.Add new_group
            current_group <- Some new_group
        
        for line in this.GetMetaList() do
            if line.StartsWith("#") then
                let group_name = line.TrimStart('#').Trim()
                start_new_group(group_name)
            else
                let wordlist_name = line.Trim()
                match current_group with
                | Some group -> this.TryLoadWordlist(group, wordlist_name)
                | None -> Console.WriteLine(sprintf "Wordlist '%s' is not part of a group" wordlist_name, Color.Red)

    static member FromDirectory(path: string) : WordBank =
        let words = WordBank(path)
        words.Reload()
        words

    member this.Entries : IReadOnlyList<WordlistEntry> = entries.AsReadOnly()
    member this.Groups : IReadOnlyList<WordlistGroup> = groups.AsReadOnly()

    member this.WriteToDirectory() : unit =
        let ensure_directory_exists_and_empty() : unit =
            Directory.CreateDirectory(path) |> ignore
            Directory.EnumerateFiles(path)
            |> Seq.where(fun file -> Path.GetExtension(file).ToLower() = ".wordlist")
            |> Seq.iter File.Delete

        let write_wordlist_meta() : unit =
            let wordlist_meta_lines = seq {
                for group in this.Groups do
                    yield $"# {group.Name}"
                    for list in group.Lists do
                        yield list
            }
            File.WriteAllLines(Path.Combine(path, "wordlists.meta"), wordlist_meta_lines)
            
        let write_wordlist(wordlist_name: string, entries: WordlistEntry seq) : unit =
            let wordlist_path = Path.Combine(path, wordlist_name + ".wordlist")
            let entries_as_strings = entries |> Seq.map _.Item.ToString()
            File.WriteAllLines(wordlist_path, entries_as_strings)
        
        ensure_directory_exists_and_empty()
        write_wordlist_meta()
        for wordlist_name, entries in this.Entries |> Seq.groupBy _.Source.File do
            write_wordlist(wordlist_name, entries)

    member this.WriteToStream(stream: Stream) : unit =
        let wordlist_to_entries =
            this.Entries
            |> Seq.groupBy _.Source.File
            |> Seq.map (fun (wordlist_name, entries) -> (wordlist_name, Array.ofSeq entries))
            |> Map.ofSeq
            
        use bw = new BinaryWriter(stream, Text.Encoding.UTF8, true)
        bw.Write(groups.Count)
        
        let write_wordlist(wordlist_name: string, entries: WordlistEntry array) : unit =
            bw.Write(wordlist_name)
            bw.Write(entries.Length)
            for entry in entries do
                bw.Write(entry.Item.ToString())
        
        let write_group(group: WordlistGroup) : unit =
            bw.Write(group.Name)
            bw.Write(group.Lists.Count)
            for wordlist_name in group.Lists do
                let entries = wordlist_to_entries.[wordlist_name]
                write_wordlist(wordlist_name, entries)
            
        for group in this.Groups do
            write_group(group)

    member this.ReadFromStream(stream: Stream) : unit =
        if entries.Count > 0 && OperatingSystem.IsWindows() then failwith "This will OVERWRITE your current entries! Guard rail for now"
        
        groups.Clear()
        entries.Clear()
        deduplicate_de.Clear()
        deduplicate_en.Clear()
        
        use br = new BinaryReader(stream, Text.Encoding.UTF8)
        
        let read_wordlist(group_name: string) : string =
            let wordlist_name = br.ReadString()
            let entry_count = br.ReadInt32()
            for line_n = 0 to entry_count - 1 do
                match this.TryAddLine({ Group = group_name; File = wordlist_name }, line_n, br.ReadString()) with
                | Ok () -> ()
                | Error reason -> Console.WriteLine(reason)
            wordlist_name
        
        let read_group() : WordlistGroup =
            let group_name = br.ReadString()
            let group_list_count = br.ReadInt32()
            let group_lists = ResizeArray<string>(group_list_count)
            for _ = 1 to group_list_count do
                let wordlist_name = read_wordlist(group_name)
                group_lists.Add(wordlist_name)
            { Name = group_name; Lists = group_lists }
        
        let group_count = br.ReadInt32()
        for _ = 1 to group_count do
            let next_group = read_group()
            groups.Add(next_group)
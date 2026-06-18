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
    
    let report_duplicate(language: string, source: Source, line_n: int, item: WordlistItem, existing_conflict: DuplicateEntry) : unit =
        let is_identical = item = existing_conflict.Item
        if existing_conflict.SourceFile <> source.File || existing_conflict.Line <> line_n then
            failwithf "%s %s definition! \n %s (%s:%i)\n %s (%s:%i)"
                (if is_identical then "Duplicate" else "Conflict with")
                language
                (item.ToString()) source.File line_n
                (existing_conflict.Item.ToString()) existing_conflict.SourceFile existing_conflict.Line
                
    let check_language_duplicate(language: string, already_seen: Dictionary<string, DuplicateEntry>, source: Source, line_n: int, ascii_identifier: string, item: WordlistItem) =
        match already_seen.TryGetValue(ascii_identifier) with
        | true, existing_conflict -> report_duplicate(language, source, line_n, item, existing_conflict)
        | false, _ -> already_seen.Add(ascii_identifier, { SourceFile = source.File; Line = line_n; Item = item })

    member this.CheckDuplicate(source: Source, line_n: int, vocab: Vocab, item: WordlistItem) : unit =
        let de_ascii_identifier = vocab.DeutschAsciiIdentifier
        check_language_duplicate("German", deduplicate_de, source, line_n, de_ascii_identifier, item)

        let en_ascii_identifier = vocab.EnglishAsciiIdentifier
        check_language_duplicate("English", deduplicate_en, source, line_n, en_ascii_identifier, item)

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
            if line = "" || line.StartsWith "#" then
                () // reserved for comments for now
            this.AddVocab(source, line_n, line)
            Ok()
        with err -> Error err.Message

    member this.ReadFile(source: Source, file_path: string) : unit =
        File.ReadAllLines(file_path)
        |> Seq.where (fun line -> line.Trim() <> "")
        |> Seq.iteri (fun i line ->
            match this.TryAddLine(source, i, line) with
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
        Directory.CreateDirectory(path) |> ignore
        Directory.EnumerateFiles(path)
        |> Seq.where(fun file -> Path.GetExtension(file).ToLower() = ".wordlist")
        |> Seq.iter File.Delete

        let wordlist_meta = seq {
            for group in this.Groups do
                yield $"# {group.Name}"
                for list in group.Lists do
                    yield list
        }
        File.WriteAllLines(Path.Combine(path, "wordlists.meta"), wordlist_meta)
        this.Entries
        |> Seq.groupBy _.Source.File
        |> Seq.iter (fun (file, entries) ->
            File.WriteAllLines(Path.Combine(path, file + ".wordlist"), entries |> Seq.map _.Item.ToString())
        )

    member this.WriteToStream(stream: Stream) : unit =
        let grouped = this.Entries |> Seq.groupBy _.Source.File |> Seq.map (fun (file, items) -> (file, Array.ofSeq items)) |> Map.ofSeq
        use bw = new BinaryWriter(stream, Text.Encoding.UTF8, true)
        bw.Write(groups.Count)
        for group in this.Groups do
            bw.Write(group.Name)
            bw.Write(group.Lists.Count)
            for list in group.Lists do
                bw.Write(list)
                let entries = grouped.[list]
                bw.Write(entries.Length)
                for entry in entries do
                    bw.Write(entry.Item.ToString())

    member this.ReadFromStream(stream: Stream) : unit =
        if entries.Count > 0 && OperatingSystem.IsWindows() then failwith "This will OVERWRITE your current entries! Guard rail for now"
        groups.Clear()
        entries.Clear()
        deduplicate_de.Clear()
        deduplicate_en.Clear()
        use br = new BinaryReader(stream, Text.Encoding.UTF8)
        let group_count = br.ReadInt32()
        for _ = 0 to group_count - 1 do
            let group_name = br.ReadString()
            let group_list_count = br.ReadInt32()
            let group_lists = ResizeArray<string>()
            for _ = 0 to group_list_count - 1 do
                let list_name = br.ReadString()
                let entry_count = br.ReadInt32()
                for i = 0 to entry_count - 1 do
                    match this.TryAddLine({ Group = group_name; File = list_name }, i, br.ReadString()) with
                    | Ok () -> ()
                    
                    | Error reason -> Console.WriteLine(reason)
                group_lists.Add(list_name)
            groups.Add({ Name = group_name; Lists = group_lists })

    // todo: this is a UI function, move it
    //member this.Categorise() : unit =
    //    let options = this.Groups |> Seq.collect _.Lists |> Seq.except ["uncategorised"] |> Seq.toArray
    //    for entry in this.Entries |> Seq.where(fun e -> e.Source.File = "uncategorised") do
    //        Console.Clear()
    //        Console.WriteLine()
    //        Console.WriteLine(entry.Item.HighlightString())
    //        Console.WriteLine()
    //        for i, option in Array.indexed options do
    //            Console.Write($"[%02i{i}]", Color.LightGray, Color.SlateGray)
    //            Console.WriteLine(" " + option)
    //        match Int32.TryParse(Console.ReadLine()) with
    //        | true, n when n >= 0 && n < options.Length ->
    //            File.AppendAllLines(Path.Combine(path, options.[n] + ".wordlist"), [entry.Item.ToString()])
    //        | _ -> ()
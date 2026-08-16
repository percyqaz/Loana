namespace Loana.Data

open System
open System.Collections.Generic
open System.IO
open Loana.Language

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
type Source = { Group: string; WordlistName: string }

type WordlistEntry = { mutable Source: Source; Item: WordlistItem }
type WordlistGroup = { Name: string; WordlistNames: ResizeArray<string> }
type DuplicateEntry = { WordlistName: string; LineNumber: int; Item: WordlistItem }

type WordlistError =
    {
        WordlistName: string
        LineNumber: int
        Line: string
        Reason: string
    }

type WordBank() =

    let groups = ResizeArray<WordlistGroup>()
    let entries = ResizeArray<WordlistEntry>()
    let errors = ResizeArray<WordlistError>()
    let meta_errors = ResizeArray<string>()

    let deduplicate_de = Dictionary<string, DuplicateEntry>()
    let deduplicate_en = Dictionary<string, DuplicateEntry>()

    member private this.CheckDuplicate(source: Source, line_n: int, vocab: Vocab, item: WordlistItem) : unit =

        let report_duplicate (language: string, existing_conflict: DuplicateEntry) : unit =
            let is_identical = item = existing_conflict.Item

            if existing_conflict.WordlistName <> source.WordlistName || existing_conflict.LineNumber <> line_n then
                failwithf
                    "%s %s definition! %s (%s:%i)"
                    (if is_identical then "Duplicate" else "Conflicting")
                    language
                    (existing_conflict.Item.ToString())
                    existing_conflict.WordlistName
                    existing_conflict.LineNumber

        let check_language_duplicate (language: string, already_seen: Dictionary<string, _>, ascii_identifier: string) =
            match already_seen.TryGetValue(ascii_identifier) with
            | true, existing_conflict -> report_duplicate(language, existing_conflict)
            | false, _ ->
                already_seen.Add(
                    ascii_identifier,
                    { WordlistName = source.WordlistName; LineNumber = line_n; Item = item }
                )

        check_language_duplicate("German", deduplicate_de, vocab.DeutschAsciiIdentifier)
        check_language_duplicate("English", deduplicate_en, vocab.EnglishAsciiIdentifier)

    member private this.AddVocab(source: Source, line_n: int, line: string) : WordlistEntry =
        let t = TaggedVocab.FromString(line)

        let inline add_verb () : WordlistEntry =
            let verb = Verb.FromTaggedVocab(t)

            match verb.PastParticiple with
            | KnownValue pp -> this.CheckDuplicate(source, line_n, pp, Verb verb)
            | _ -> ()

            this.CheckDuplicate(source, line_n, t.Vocab, Verb verb)
            { Item = Verb verb; Source = source }

        let inline add_noun () : WordlistEntry =
            let noun = Noun.FromTaggedVocab(t)

            match noun.Plural with
            | KnownValue plural -> this.CheckDuplicate(source, line_n, plural, Noun noun)
            | _ -> ()

            this.CheckDuplicate(source, line_n, t.Vocab, Noun noun)
            { Item = Noun noun; Source = source }

        let inline add_vocab () : WordlistEntry =
            let vocab = t.Vocab
            this.CheckDuplicate(source, line_n, vocab, Vocab vocab)
            { Item = Vocab vocab; Source = source }

        let entry =
            if t.Vocab.LooksLikeAVerb then add_verb()
            elif t.Vocab.LooksLikeANoun && t.Tags <> [] then add_noun()
            else add_vocab()

        entries.Add(entry)
        entry

    member private this.TryAddLine(source: Source, line_n: int, line: string) : Result<WordlistEntry, WordlistError> =
        try
            Ok(this.AddVocab(source, line_n, line))
        with err ->
            let error =
                {
                    WordlistName = source.WordlistName
                    LineNumber = line_n
                    Line = line
                    Reason = err.Message
                }

            errors.Add(error)
            Error(error)

    member this.AddWordList(source: Source, lines: string array) : unit =

        let inline get_or_create_group () : WordlistGroup =
            match groups |> Seq.tryFind(fun g -> g.Name = source.Group) with
            | None ->
                let group = { Name = source.Group; WordlistNames = ResizeArray() }
                groups.Add(group)
                group
            | Some existing_group -> existing_group

        let inline ensure_wordlist_added_to_group () : unit =
            let group = get_or_create_group()

            if not(group.WordlistNames.Contains(source.WordlistName)) then
                group.WordlistNames.Add(source.WordlistName)

        let inline try_add_line (line_n: int, line: string) : unit =
            let is_comment = line.Trim() = ""

            if not(is_comment) then
                this.TryAddLine(source, line_n, line) |> ignore

        ensure_wordlist_added_to_group()

        lines |> Seq.iteri(fun line_n line -> try_add_line(line_n, line))

    member this.MoveAfter(entry_to_move: WordlistEntry, relative_to: WordlistEntry) : unit =
        let target_index = entries.IndexOf(relative_to)
        let source_index = entries.IndexOf(entry_to_move)

        let insert_index =
            if target_index > source_index then target_index else target_index + 1

        if target_index >= 0 && entries.Remove(entry_to_move) then
            entries.Insert(insert_index, entry_to_move)
            entry_to_move.Source <- relative_to.Source

    member this.Clear() : unit =
        groups.Clear()
        entries.Clear()
        errors.Clear()
        deduplicate_de.Clear()
        deduplicate_en.Clear()

    member this.ReadFromStream(stream: Stream) : unit =

        use br = new BinaryReader(stream, Text.Encoding.UTF8)

        let inline read_wordlist (group_name: string) : string =
            let wordlist_name = br.ReadString()
            let entry_count = br.ReadInt32()

            for line_n = 0 to entry_count - 1 do
                let line = br.ReadString()

                this.TryAddLine({ Group = group_name; WordlistName = wordlist_name }, line_n, line) |> ignore

            wordlist_name

        let inline read_group () : WordlistGroup =
            let group_name = br.ReadString()
            let group_list_count = br.ReadInt32()
            let group_lists = ResizeArray<string>(group_list_count)

            for _ = 1 to group_list_count do
                let wordlist_name = read_wordlist(group_name)

                if group_lists.Contains(wordlist_name) then
                    failwithf "duplicate word list '%s'" group_name

                group_lists.Add(wordlist_name)

            { Name = group_name; WordlistNames = group_lists }

        if entries.Count > 0 && OperatingSystem.IsWindows() then
            failwith "This will OVERWRITE your current entries! Guard rail for now"

        this.Clear()
        let group_count = br.ReadInt32()

        for _ = 1 to group_count do
            let next_group = read_group()
            groups.Add(next_group)

    member this.WriteToStream(stream: Stream) : unit =
        use bw = new BinaryWriter(stream, Text.Encoding.UTF8, true)

        let wordlist_to_entries =
            this.Entries
            |> Seq.groupBy _.Source.WordlistName
            |> Seq.map(fun (wordlist_name, entries) -> (wordlist_name, Array.ofSeq entries))
            |> Map.ofSeq

        let wordlist_to_errors =
            this.Errors
            |> Seq.groupBy _.WordlistName
            |> Seq.map(fun (wordlist_name, errors) -> (wordlist_name, Array.ofSeq errors))
            |> Map.ofSeq

        let inline write_wordlist (wordlist_name: string) : unit =
            let entries = wordlist_to_entries.[wordlist_name]

            let errors =
                Map.tryFind wordlist_name wordlist_to_errors |> Option.defaultWith(fun () -> [||])

            bw.Write(wordlist_name)
            bw.Write(entries.Length + errors.Length)

            for entry in entries do
                bw.Write(entry.Item.ToString())

            for error in errors do
                bw.Write(error.Line)


        let inline write_group (group: WordlistGroup) : unit =
            bw.Write(group.Name)
            bw.Write(group.WordlistNames.Count)

            for wordlist_name in group.WordlistNames do
                write_wordlist(wordlist_name)

        bw.Write(groups.Count)

        for group in this.Groups do
            write_group(group)

    member this.ReadFromDirectory(path: string) : unit =

        let inline get_meta_list () =
            let meta_list = Path.Combine(path, "wordlists.meta")

            if File.Exists(meta_list) |> not then
                meta_errors.Add(sprintf "'%s' doesn't exist!" meta_list)
                [||]
            else
                File.ReadAllLines(meta_list)

        let inline load_wordlist_file (source: Source) : unit =
            let wordlist_path = Path.Combine(path, source.WordlistName + ".wordlist")

            if Path.Exists(wordlist_path) then
                this.AddWordList(source, File.ReadAllLines(wordlist_path))
            else
                meta_errors.Add(sprintf "could not find wordlist '%s' at %s" source.WordlistName wordlist_path)

        this.Clear()
        let mutable current_group: string option = None

        for line in get_meta_list() do
            if line.StartsWith("#") then
                let group_name = line.TrimStart('#').Trim()
                current_group <- Some group_name
            else
                let wordlist_name = line.Trim()

                match current_group with
                | Some group -> load_wordlist_file({ Group = group; WordlistName = wordlist_name })
                | None -> meta_errors.Add(sprintf "wordlist '%s' is not part of a group" wordlist_name)

    member this.WriteToDirectory(path: string) : unit =

        let inline ensure_directory_exists_and_empty () : unit =
            Directory.CreateDirectory(path) |> ignore

            Directory.EnumerateFiles(path)
            |> Seq.where(fun file -> Path.GetExtension(file).ToLower() = ".wordlist")
            |> Seq.iter File.Delete

        let inline write_wordlist (wordlist_name: string, entries: WordlistEntry seq) : unit =
            let wordlist_path = Path.Combine(path, wordlist_name + ".wordlist")
            let entries_as_strings = entries |> Seq.map _.Item.ToString()

            let errors =
                this.Errors |> Seq.filter(fun error -> error.WordlistName = wordlist_name) |> Seq.map _.Line

            File.WriteAllLines(wordlist_path, Seq.concat [ entries_as_strings; errors ])

        let inline write_wordlist_meta () : unit =
            let wordlist_meta_lines =
                seq {
                    for group in this.Groups do
                        yield $"# {group.Name}"

                        for list in group.WordlistNames do
                            yield list
                }

            File.WriteAllLines(Path.Combine(path, "wordlists.meta"), wordlist_meta_lines)

        ensure_directory_exists_and_empty()
        write_wordlist_meta()

        for wordlist_name, entries in this.Entries |> Seq.groupBy _.Source.WordlistName do
            write_wordlist(wordlist_name, entries)

    static member CreateFromDirectory(path: string) : WordBank =
        let words = WordBank()
        words.ReadFromDirectory(path)
        words

    member this.Entries: IReadOnlyList<WordlistEntry> = entries.AsReadOnly()
    member this.Groups: IReadOnlyList<WordlistGroup> = groups.AsReadOnly()
    member this.Errors: IReadOnlyList<WordlistError> = errors.AsReadOnly()
    member this.MetaErrors: IReadOnlyList<string> = meta_errors.AsReadOnly()

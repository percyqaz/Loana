namespace Loana.Data

open System
open System.Collections.Generic
open System.Drawing
open System.IO
open Loana.Language

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

    let internal parse_verb_inner(vocab: Vocab, tags: string list) : Verb =
        let mutable mtags = tags
        let mutable quizzes: VerbQuiz list = []
        let mutable pp: Knowledge<Vocab> = if tags <> [] then Nothing else ToBeDetermined
        let mutable dative = false

        while mtags <> [] do
            let next = mtags.Head
            mtags <- mtags.Tail
            match next with
            | "pa" | "pr" | "im" ->
                quizzes <- quizzes @ [VerbQuiz.Parse(next)]
            | "dat" ->
                if dative then failwith "Dative specified twice"
                if quizzes <> [] then failwith "Dative must be specified before quizzes"
                dative <- true
            | "pp" ->
                pp <- Something (Vocab.Parse (String.concat " " mtags))
                mtags <- []
            | _ -> failwithf "Unrecognised tag '%s' for noun: %O" next vocab

        {
            Infinitive = vocab
            PastParticiple = pp
            Quizzes = quizzes |> List.distinct
            Dative = dative
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
    override this.ToString() =
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

    member this.CheckDuplicate(source: Source, line_n: int, vocab: Vocab, item: WordlistItem) : unit =

        let ded_de = vocab.Key
        if deduplicate_de.ContainsKey(ded_de) then
            let existing_conflict = deduplicate_de.[ded_de]
            let identical = item = existing_conflict.Item

            if source.File = "uncategorised" && identical then
                failwith ""

            if existing_conflict.SourceFile <> source.File || existing_conflict.Line <> line_n then
                failwithf "%s German definition! \n %s (%s:%i)\n %s (%s:%i)"
                    (if identical then "Duplicate" else "Conflict with")
                    (item.ToString()) source.File line_n
                    (existing_conflict.Item.ToString()) existing_conflict.SourceFile existing_conflict.Line
        else
            deduplicate_de.Add(ded_de, { SourceFile = source.File; Line = line_n; Item = item })

        let ded_en = vocab.EnglishKey
        if deduplicate_en.ContainsKey(ded_en) then
            let existing_conflict = deduplicate_en.[ded_en]
            let identical = item = existing_conflict.Item

            if existing_conflict.SourceFile <> source.File || existing_conflict.Line <> line_n then
                failwithf "%s English definition! \n %s (%s:%i)\n %s (%s:%i)"
                    (if identical then "Duplicate" else "Conflict with")
                    (item.ToString()) source.File line_n
                    (existing_conflict.Item.ToString()) existing_conflict.SourceFile existing_conflict.Line
        else
            deduplicate_en.Add(ded_en, { SourceFile = source.File; Line = line_n; Item = item })

    member this.AddVocab(source: Source, line_n: int, line: string) : unit =
        let v, tags = Wordlist.parse_core line

        if v.DetectVerb then
            let verb = Wordlist.parse_verb_inner(v, tags)
            match verb.PastParticiple with
            | Something pp -> this.CheckDuplicate(source, line_n, pp, Verb verb)
            | _ -> ()
            this.CheckDuplicate(source, line_n, v, Verb verb)
            entries.Add { Item = Verb verb; Source = source }

        elif v.DetectNoun && tags <> [] then
            let noun = Wordlist.parse_noun_inner(v, tags)
            match noun.Plural with
            | Something plural -> this.CheckDuplicate(source, line_n, plural, Noun noun)
            | _ -> ()
            this.CheckDuplicate(source, line_n, v, Noun noun)
            entries.Add { Item = Noun noun; Source = source }

        else
            this.CheckDuplicate(source, line_n, v, Vocab v)
            entries.Add { Item = Vocab v; Source = source }

    member this.TryAdd(source: Source, line_n: int, line: string) : Result<unit, string> =
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
            match this.TryAdd(source, i, line) with
            | Ok() -> ()
            | Error "" -> ()
            | Error reason ->
                Console.Write($" {source.File}: ", Color.LightBlue, Color.FromArgb 0x202020)
                Console.WriteLine(" " + reason, Color.Red)
        )

    member private this.Reload() : unit =
        groups.Clear()
        entries.Clear()
        deduplicate_de.Clear()
        deduplicate_en.Clear()

        let meta_list = Path.Combine(path, "wordlists.meta")
        if File.Exists(meta_list) |> not then
            Console.WriteLine(sprintf "'%s' doesn't exist!" meta_list, Color.Red)
        else
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

    static member FromDirectory(path: string) : WordBank =
        let words = WordBank(path)
        words.Reload()
        words

    member this.Entries : IReadOnlyList<WordlistEntry> = entries.AsReadOnly()
    member this.Groups : IReadOnlyList<WordlistGroup> = groups.AsReadOnly()

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
namespace Loana.Database

open System.IO
open Loana

type Wordlist =
    {
        Nouns: Noun array
        Verbs: Verb array
        Vocab: Vocab array
    }

module Wordlist =

    let private parse_noun_inner(vocab: Vocab, tags: string list) : Noun =
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

    let private parse_core (line: string) : Vocab * string list =
        if line = "" then failwith "Cannot parse empty line as a noun"

        let split = line.Split(":", 2, System.StringSplitOptions.TrimEntries ||| System.StringSplitOptions.RemoveEmptyEntries)
        let tags =
            if split.Length = 2 then
                split.[1].Split(" ", System.StringSplitOptions.TrimEntries ||| System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
            else
                []
        let vocab = Vocab.Parse split.[0]
        vocab, tags

    let parse_noun : string -> Noun =
        parse_core >> parse_noun_inner

    let parse_vocab : string -> Vocab =
        parse_core >> fst
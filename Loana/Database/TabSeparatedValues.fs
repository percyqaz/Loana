namespace Loana.Database

open System.IO
open Loana

module TabSeparatedValues =

    let read(sr: StreamReader) : string array seq =
        seq {
            while not sr.EndOfStream do
                yield sr.ReadLine().Split("\t", System.StringSplitOptions.TrimEntries)
            sr.Dispose()
        }

    let read_file(path: string) =
        let stream = File.Open(path, FileMode.OpenOrCreate)
        let sr = new StreamReader(stream, System.Text.Encoding.UTF8, true, leaveOpen = false)
        read sr

    let read_noun (values: string array) : Result<Noun, string> =
        if values.Length < 3 then Error "Not enough values!" else

        let gender = Gender.Parse(values[1])
        let extra = if values.Length > 3 then Some values.[3] else None
        let extra = if extra = Some "" then None else extra
        {
            Deutsch = values.[0]
            English = values.[2]
            Guts =
                match gender with
                | Gender.Masculine -> Masculine { Plural = extra }
                | Gender.Feminine -> Feminine { Plural = extra }
                | Gender.Neuter -> Neuter { Plural = extra }
                | Gender.Plural -> Plural { Singular = extra }
        } |> Ok

    let write_noun (noun: Noun) : string array =
        [|
            noun.Deutsch
            noun.Guts.Gender.ToString()
            noun.English
            match noun.Guts with
            | Masculine x -> x.Plural |> Option.defaultValue ""
            | Feminine x -> x.Plural |> Option.defaultValue ""
            | Neuter x -> x.Plural |> Option.defaultValue ""
            | Plural x -> x.Singular |> Option.defaultValue ""
        |]

    let write (sw: StreamWriter, data: string array seq) : unit =
        for row in data do
            sw.WriteLine(String.concat "\t" row)

    let write_file(path: string, data: string array seq) : unit =
        let stream = File.Open(path, FileMode.Create)
        let sw = new StreamWriter(stream, System.Text.Encoding.UTF8, leaveOpen = false)
        write (sw, data)
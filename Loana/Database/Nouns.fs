namespace Loana.Database

open Loana

type NounDatabase =
    {
        Nouns: Noun array
    }

    static member FromFile(path: string) =
        { Nouns =
            TabSeparatedValues.read_file(path)
            |> Seq.map TabSeparatedValues.read_noun
            |> Seq.map Result.toOption
            |> Seq.choose id
            |> Array.ofSeq
        }

    member this.RunCheck(func: Noun -> Noun option) : NounDatabase =
        {
            Nouns =
                this.Nouns
                |> Seq.choose func
                |> Array.ofSeq
        }

    static member LowercaseEnglish =
        fun (noun : Noun) -> Some { noun with English = noun.English.ToLower() }

    static member CleanDuplicates() =
        let mutable seen = Set.empty
        fun noun ->
            if seen.Contains(noun.ToString()) then
                printfn "%A is a duplicate of %O" noun noun
                None
            else
                seen <- seen.Add(noun.ToString())
                Some noun

    member this.ToFile(path: string) : unit =
        TabSeparatedValues.write_file(path, this.Nouns |> Seq.map TabSeparatedValues.write_noun)
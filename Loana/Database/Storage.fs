namespace Loana.Database

open System.IO
open Loana

[<AbstractClass>]
type DbFile<'T>(path: string) =

    abstract member Version : int
    abstract member ReadItem : int * BinaryReader -> 'T
    abstract member WriteItem : 'T * BinaryWriter -> unit

    member this.Read() : 'T array =
        let stream = File.Open(path, FileMode.OpenOrCreate)
        use br = new BinaryReader(stream, System.Text.Encoding.UTF8, leaveOpen = false)

        if stream.Position = stream.Length then
            printfn "File '%s' is empty" path
            [||]
        else

        let version = br.ReadInt32()
        let count = br.ReadInt32()
        try
            seq {
                for _ = 1 to count do
                    yield this.ReadItem(version, br)
            }
            |> Array.ofSeq
        with
        | :? EndOfStreamException -> reraise() // todo

    member this.Write(items: 'T array) =
        let bak_path = path + ".bak"
        let temp_path = path + ".tmp"
        let stream = File.Open(temp_path, FileMode.Create)
        let bw = new BinaryWriter(stream, System.Text.Encoding.UTF8, leaveOpen = false)
        bw.Write(this.Version)
        bw.Write(items.Length)
        for item in items do
            this.WriteItem(item, bw)
        bw.Dispose()

        try File.Delete(bak_path) with _ -> ()
        File.Move(path, bak_path)
        File.Move(temp_path, path)

type NounFile(path: string) =
    inherit DbFile<Noun>(path: string)

    override this.Version = 1

    override this.ReadItem (version: int, br: BinaryReader): Noun =
        match version with
        | 1 ->
            {
                Deutsch = br.ReadString()
                English = br.ReadString()
                EnglishAlternatives =
                    let count = br.Read7BitEncodedInt()
                    List.init count (fun _ -> br.ReadString())
                Guts =
                    let gender =
                        match br.ReadByte() with
                        | 3uy -> Gender.Plural
                        | 2uy -> Gender.Neuter
                        | 1uy -> Gender.Feminine
                        | _ -> Gender.Masculine

                    let other_form =
                        match br.ReadByte() with
                        | 1uy -> Something (br.ReadString())
                        | 2uy -> Nothing
                        | _ -> ToBeDetermined

                    match gender with
                    | Gender.Plural -> Plural { Singular = other_form }
                    | Gender.Neuter -> Neuter { Plural = other_form }
                    | Gender.Feminine -> Feminine { Plural = other_form }
                    | Gender.Masculine -> Masculine { Plural = other_form }
            }
        | _ ->
            failwithf "Unrecognised version %i" version

    override this.WriteItem(noun: Noun, bw: BinaryWriter): unit =
        bw.Write(noun.Deutsch)
        bw.Write(noun.English)
        bw.Write7BitEncodedInt(noun.EnglishAlternatives.Length)
        for alt in noun.EnglishAlternatives do
            bw.Write(alt)

        let write_other_form(k: Knowledge<string>) =
            match k with
            | ToBeDetermined -> bw.Write 0uy
            | Something v -> bw.Write 1uy; bw.Write v
            | Nothing -> bw.Write 2uy

        match noun.Guts with
        | Plural { Singular = singular } -> bw.Write 3uy; write_other_form singular
        | Neuter { Plural = plural } -> bw.Write 2uy; write_other_form plural
        | Feminine { Plural = plural } -> bw.Write 1uy; write_other_form plural
        | Masculine { Plural = plural } -> bw.Write 0uy; write_other_form plural

type VerbFile(path: string) =
    inherit DbFile<Verb>(path: string)

    override this.Version = 1

    override this.ReadItem (version: int, br: BinaryReader): Verb =
        match version with
        | 1 ->
            {
                Deutsch = br.ReadString()
                English = br.ReadString()
                EnglishAlternatives =
                    let count = br.Read7BitEncodedInt()
                    List.init count (fun _ -> br.ReadString())
                Tag =
                    match br.ReadByte() with
                    | 4uy -> VerbTag.Reciprocal
                    | 3uy -> VerbTag.Reflexive
                    | 2uy -> VerbTag.Transitive
                    | 1uy -> VerbTag.Intransitive
                    | _ -> VerbTag.None
                Separable = br.ReadBoolean()
                Inflections =
                    let read_verb_person () : VerbPerson =
                        match br.ReadByte() with
                        | 4uy -> ThirdSingular
                        | 3uy -> SecondPlural
                        | 2uy -> SecondSingular
                        | 1uy -> FirstThirdPluralFormal
                        | _ -> FirstSingular

                    let read_inflection() : VerbInflection =
                        match br.ReadByte() with
                        | 3uy -> Imperative
                        | 2uy -> PastParticiple
                        | 1uy -> SimplePast (read_verb_person())
                        | _ -> Present (read_verb_person())

                    let read_inflected() : (string * string) option =
                        match br.ReadByte() with
                        | 1uy -> Some (br.ReadString(), br.ReadString())
                        | _ -> None

                    let count = br.Read7BitEncodedInt()
                    seq {
                        for _ = 1 to count do
                            yield read_inflection(), read_inflected()
                    }
                    |> Map.ofSeq
            }
        | _ ->
            failwithf "Unrecognised version %i" version

    override this.WriteItem(verb: Verb, bw: BinaryWriter): unit =
        bw.Write(verb.Deutsch)
        bw.Write(verb.English)
        bw.Write7BitEncodedInt(verb.EnglishAlternatives.Length)
        for alt in verb.EnglishAlternatives do
            bw.Write(alt)

        match verb.Tag with
        | VerbTag.None -> bw.Write 0uy
        | VerbTag.Intransitive -> bw.Write 1uy
        | VerbTag.Transitive -> bw.Write 2uy
        | VerbTag.Reflexive -> bw.Write 3uy
        | VerbTag.Reciprocal -> bw.Write 4uy

        bw.Write(verb.Separable)

        let write_verb_person(p: VerbPerson) =
            match p with
            | FirstSingular -> bw.Write 0uy
            | FirstThirdPluralFormal -> bw.Write 1uy
            | SecondSingular -> bw.Write 2uy
            | SecondPlural -> bw.Write 3uy
            | ThirdSingular -> bw.Write 4uy

        let write_inflection(i: VerbInflection) =
            match i with
            | Present p -> bw.Write 0uy; write_verb_person p
            | SimplePast p -> bw.Write 1uy; write_verb_person p
            | PastParticiple -> bw.Write 2uy
            | Imperative -> bw.Write 3uy

        for (key, value) in Map.toSeq verb.Inflections do
            write_inflection key
            match value with
            | Some (de, en) -> bw.Write 1uy; bw.Write de; bw.Write en
            | None -> bw.Write 0uy
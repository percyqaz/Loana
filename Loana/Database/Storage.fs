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

[<AutoOpen>]
module internal Helpers =
    let read_translation(br: BinaryReader) : Vocab =
        {
            Deutsch = br.ReadString()
            English = Annotation.Parse <| br.ReadString()
            EnglishAlternatives =
                let count = br.Read7BitEncodedInt()
                List.init count (fun _ -> Annotation.Parse <| br.ReadString())
        }

    let write_translation (bw: BinaryWriter) (translation: Vocab) =
        bw.Write(translation.Deutsch)
        bw.Write(translation.English.ToString())
        bw.Write7BitEncodedInt(translation.EnglishAlternatives.Length)
        for alt in translation.EnglishAlternatives do
            bw.Write(alt.ToString())

type NounFile(path: string) =
    inherit DbFile<Noun>(path: string)

    override this.Version = 1

    override this.ReadItem (version: int, br: BinaryReader): Noun =
        match version with
        | 1 ->
            {
                Translation = read_translation(br)
                Guts =
                    let gender =
                        match br.ReadByte() with
                        | 3uy -> Gender.Plural
                        | 2uy -> Gender.Neuter
                        | 1uy -> Gender.Feminine
                        | _ -> Gender.Masculine

                    let plural_form() =
                        match br.ReadByte() with
                        | 1uy -> Something (read_translation(br))
                        | 2uy -> Nothing
                        | _ -> ToBeDetermined

                    match gender with
                    | Gender.Plural -> Plural
                    | Gender.Neuter -> Neuter (plural_form())
                    | Gender.Feminine -> Feminine (plural_form())
                    | Gender.Masculine -> Masculine (plural_form())
            }
        | _ ->
            failwithf "Unrecognised version %i" version

    override this.WriteItem(noun: Noun, bw: BinaryWriter): unit =
        write_translation bw noun.Translation

        let write_plural_form(k: Knowledge<Vocab>) =
            match k with
            | ToBeDetermined -> bw.Write 0uy
            | Something v -> bw.Write 1uy; write_translation bw v
            | Nothing -> bw.Write 2uy

        match noun.Guts with
        | Plural -> bw.Write 3uy
        | Neuter plural -> bw.Write 2uy; write_plural_form plural
        | Feminine plural -> bw.Write 1uy; write_plural_form plural
        | Masculine plural -> bw.Write 0uy; write_plural_form plural

type VerbFile(path: string) =
    inherit DbFile<Verb>(path: string)

    override this.Version = 1

    override this.ReadItem (version: int, br: BinaryReader): Verb =
        match version with
        | 1 ->
            {
                Infinitive = read_translation br
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
        write_translation bw verb.Infinitive

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

        let inflections = Map.toArray verb.Inflections
        bw.Write7BitEncodedInt inflections.Length
        for (key, value) in inflections do
            write_inflection key
            match value with
            | Some (de, en) -> bw.Write 1uy; bw.Write de; bw.Write en
            | None -> bw.Write 0uy
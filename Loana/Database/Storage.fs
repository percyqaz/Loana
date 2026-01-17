namespace Loana.Database

open System.IO

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

// to be used for card progress data only
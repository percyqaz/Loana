namespace Loana

open System.Runtime.CompilerServices
open Avalonia.Media

[<Interface>]
type IOutput =
    abstract member Write: string * (IBrush | null) * (IBrush | null) -> unit
    abstract member Clear: unit -> unit

[<Extension>]
type IOutputExtensions =

    [<Extension>]
    static member Write(this: IOutput, text: string, brush: IBrush | null) = this.Write(text, brush, null)

    [<Extension>]
    static member Write(this: IOutput, text: string) = this.Write(text, null, null)

    [<Extension>]
    static member WriteLine(this: IOutput, text: string, brush: IBrush | null, background: IBrush | null) = this.Write(text + "\n", brush, background)

    [<Extension>]
    static member WriteLine(this: IOutput, text: string, brush: IBrush | null) = this.WriteLine(text, brush, null)

    [<Extension>]
    static member WriteLine(this: IOutput, text: string) = this.WriteLine(text, null, null)
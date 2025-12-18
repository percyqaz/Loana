namespace Loana.Interface

open Avalonia.Media

[<Interface>]
type IOutput =
    abstract member Write: string * (IBrush | null) -> unit
    abstract member WriteLine: string * (IBrush | null) -> unit
    abstract member Clear: unit -> unit
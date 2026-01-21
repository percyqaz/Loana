namespace Loana.CLI

open System.Runtime.CompilerServices
open System.Drawing

type Console =

    static member Clear() = System.Console.Clear()

    static member Write(text: string, fg: Color, bg: Color) = System.Console.Write (sprintf "\u001b[38;2;%d;%d;%d;48;2;%d;%d;%dm%s\u001b[0m" fg.R fg.G fg.B bg.R bg.G bg.B text)
    static member Write(text: string, fg: Color) = Console.Write(text, fg, Color.Black)
    static member Write(text: string) = Console.Write(text, Color.White, Color.Black)

    static member WriteLine(text: string, color: Color, background: Color) = Console.Write(text + "\n", color, background)
    static member WriteLine(text: string, color: Color) = Console.WriteLine(text, color, Color.Black)
    static member WriteLine(text: string) = Console.WriteLine(text, Color.White, Color.Black)
    static member WriteLine() = Console.WriteLine("", Color.White, Color.Black)
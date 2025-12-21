namespace Loana.Interface

open System
open Avalonia.Media
open Loana

type MenuContext<'T> =
    private {
        Output: IOutput
        Options: Map<string, 'T>
        Callback: Action<'T>
        mutable Selected: int
    }

    member this.Draw() : unit =
        let keys = Array.ofSeq this.Options.Keys
        this.Output.Clear()
        for i = 0 to keys.Length - 1 do
            if i = this.Selected then
                this.Output.Write(" > ")
                this.Output.Write(keys.[i], Brushes.Yellow)
                this.Output.WriteLine(" <")
            else
                this.Output.WriteLine(sprintf "%02i %s" ((keys.Length + i - this.Selected) % keys.Length) keys.[i], Brushes.LightGray)

    member this.Next(user_input: string) : unit =
        let keys = Array.ofSeq this.Options.Keys
        match Int32.TryParse(user_input) with
        | true, n ->
            this.Selected <- ((this.Selected + n) % keys.Length + keys.Length) % keys.Length
            this.Draw()
        | false, _ -> this.Callback.Invoke(this.Options.[keys.[this.Selected]])

type MenuContext =

    static member Create(options: Map<string, 'T>, callback: Action<'T>, output: IOutput) : MenuContext<'T> =
        {
            Output = output
            Options = options
            Callback = callback
            Selected = 0
        }

    static member CreateDeckPicker(callback: Action<(Cards.CardPool.CardPermutation -> bool)>, output: IOutput) : MenuContext<Cards.CardPool.CardPermutation -> bool> =
        MenuContext.Create(Cards.CardPool.DECKS, callback, output)
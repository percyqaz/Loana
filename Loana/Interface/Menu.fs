namespace Loana.Interface

open System
open Avalonia.Media
open Loana

type MenuItem<'T> =
    {
        Name: string
        Value: 'T
    }

type MenuContext<'T> =
    private {
        Output: IOutput
        Options: MenuItem<'T> array
        Callback: Action<'T>
        mutable Selected: int
    }

    member this.Draw() : unit =
        this.Output.Clear()
        for i = 0 to this.Options.Length - 1 do
            if i = this.Selected then
                this.Output.Write(" > ")
                this.Output.Write(this.Options.[i].Name, Brushes.Yellow)
                this.Output.WriteLine(" <")
            else
                this.Output.WriteLine(sprintf "%02i %s" ((this.Options.Length + i - this.Selected) % this.Options.Length) this.Options.[i].Name, Brushes.LightGray)

    member this.Next(user_input: string) : unit =
        match Int32.TryParse(user_input) with
        | true, n ->
            this.Selected <- ((this.Selected + n) % this.Options.Length + this.Options.Length) % this.Options.Length
            this.Draw()
        | false, _ -> this.Callback.Invoke(this.Options.[this.Selected].Value)

type MenuContext =

    static member Create(options: MenuItem<'T> array, callback: Action<'T>, output: IOutput) : MenuContext<'T> =
        {
            Output = output
            Options = options
            Callback = callback
            Selected = 0
        }

    static member Create(options: 'T array, labels: Func<'T, string>, callback: Action<'T>, output: IOutput) : MenuContext<'T> =
        MenuContext.Create(options |> Array.map (fun x -> { Name = labels.Invoke(x); Value = x }), callback, output)
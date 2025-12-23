namespace Loana.Interface

open System
open Avalonia.Media
open Loana

[<AbstractClass>]
type Menu (output: IOutput) =
    member internal this.Output = output
    abstract member Next: string -> bool
    abstract member Start: unit -> bool

type Submenu =
    private { mutable Menu: Menu option }

    static member Create() : Submenu = { Menu = None }

    member this.Open(menu: Menu) =
        if menu.Start() then this.Menu <- Some menu

    member this.HasMenu : bool = this.Menu.IsSome

    member this.Next(user_input: string) : bool =
        match this.Menu with
        | Some m ->
            if not (m.Next(user_input)) then
                this.Menu <- None
                false
            else
                true
        | None -> false

type SelectMenuOption = { Name: string; Menu: Func<Menu> }

type SelectMenu(options: SelectMenuOption array, output: IOutput) =
    inherit Menu(output)

    let mutable selected = 0
    let submenu = Submenu.Create()

    member private this.Draw() : unit =
        this.Output.Clear()
        for i = 0 to options.Length - 1 do
            if i = selected then
                this.Output.Write(" > ")
                this.Output.Button(options.[i].Name, "ok", Brushes.Yellow)
                this.Output.WriteLine(" <")
            else
                let offset = (options.Length + i - selected) % options.Length
                this.Output.Button(sprintf "%02i %s" offset options.[i].Name, sprintf "%i" offset, Brushes.LightGray)
                this.Output.WriteLine()

        output.WriteLine()
        output.Button(" ok ", "ok", Brushes.LightGray, Brush.Parse("#101010"))
        output.Write(" ")
        output.Button(" back ", "back", Brushes.LightGray, Brush.Parse("#101010"))

    override this.Next(user_input: string) : bool =
        if submenu.HasMenu then
            if not (submenu.Next(user_input)) then
                this.Draw()
            true
        else

        match user_input with
        | "back" -> false
        | "ok" ->
            submenu.Open(options.[selected].Menu.Invoke())
            true
        | _ ->
            match Int32.TryParse(user_input) with
            | true, n ->
                selected <- ((selected + n) % options.Length + options.Length) % options.Length
            | false, _ -> ()
            this.Draw()
            true

    override this.Start() : bool = this.Draw(); true
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
                this.Output.Write(options.[i].Name, Brushes.Yellow)
                this.Output.WriteLine(" <")
            else
                this.Output.WriteLine(sprintf "%02i %s" ((options.Length + i - selected) % options.Length) options.[i].Name, Brushes.LightGray)

    override this.Next(user_input: string) : bool =
        if submenu.HasMenu then
            if not (submenu.Next(user_input)) then
                this.Draw()
            true
        else

        match Int32.TryParse(user_input) with
        | true, n ->
            selected <- ((selected + n) % options.Length + options.Length) % options.Length
            this.Draw()
            true
        | false, _ ->
            submenu.Open(options.[selected].Menu.Invoke())
            true

    override this.Start() : bool = this.Draw(); true
namespace Loana.Interface

open System
open Avalonia.Media

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

type DummyMenu(output) =
    inherit Menu(output)

    override this.Start() = false
    override this.Next(_) = false

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

        this.Output.WriteLine()
        this.Output.Button(" ok ", "ok", Brushes.LightGray, Brush.Parse("#101010"))
        this.Output.Write(" ")
        this.Output.Button(" back ", "back", Brushes.LightGray, Brush.Parse("#101010"))

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

type EditorOption<'T> = { Name: string; Draw: 'T -> IOutput -> unit; Menu: (unit -> 'T) -> ('T -> unit) -> Menu }
type EditorMenu<'T>(options: EditorOption<'T> array, get: unit -> 'T, set: 'T -> unit, output: IOutput) =
    inherit Menu(output)

    let mutable selected = 0
    let submenu = Submenu.Create()

    member private this.Draw() : unit =
        let current_value = get()

        this.Output.Clear()
        for i = 0 to options.Length - 1 do
            if i = selected then
                this.Output.Button(options.[i].Name, "ok", Brushes.Yellow)
                this.Output.Write " "
                options.[i].Draw current_value this.Output
            else
                let offset = (options.Length + i - selected) % options.Length
                this.Output.Button(sprintf "%s" options.[i].Name, sprintf "%i" offset, Brushes.LightGray)
                this.Output.Write " "
                options.[i].Draw current_value this.Output
            this.Output.WriteLine()

        this.Output.WriteLine()
        this.Output.Button(" ok ", "ok", Brushes.LightGray, Brush.Parse("#101010"))
        this.Output.Write(" ")
        this.Output.Button(" back ", "back", Brushes.LightGray, Brush.Parse("#101010"))

    override this.Next(user_input: string) : bool =
        if submenu.HasMenu then
            if not (submenu.Next(user_input)) then
                this.Draw()
            true
        else

        match user_input with
        | "back" -> false
        | "ok" ->
            submenu.Open(options.[selected].Menu get set)
            true
        | _ ->
            match Int32.TryParse(user_input) with
            | true, n ->
                selected <- ((selected + n) % options.Length + options.Length) % options.Length
            | false, _ -> ()
            this.Draw()
            true

    override this.Start() : bool = this.Draw(); true

type EditTextFieldMenu(get: unit -> string, set: string -> unit, output: IOutput) =
    inherit Menu(output)

    member private this.Draw() : unit =
        this.Output.Clear()
        this.Output.WriteLine()
        this.Output.Write("Current value: ", Brushes.Gray)
        this.Output.WriteLine(get())

    override this.Next(user_input: string) : bool =
        set user_input
        false

    override this.Start() : bool = this.Draw(); true

type BrowserMenu<'T>(
    search: string -> 'T seq,
    display: 'T -> string,
    create: ('T -> unit) -> Menu,
    remove: 'T -> unit,
    add: 'T -> unit,
    edit: (unit -> 'T) -> ('T -> unit) -> Menu,
    save: unit -> unit,
    output: IOutput) =
    inherit Menu(output)

    let mutable search_query = ""
    let mutable search_results = [||]
    let mutable selected = 0
    let mutable current_editor = Unchecked.defaultof<_>
    let submenu = Submenu.Create()

    let refresh_search() =
        search_results <- search search_query |> Seq.truncate 20 |> Array.ofSeq
        selected <- if search_results.Length = 0 then 0 else selected % search_results.Length

    do refresh_search()

    member private this.Draw() : unit =
        this.Output.Clear()

        this.Output.Write("Search: [ ")
        this.Output.Write(search_query, Brushes.Green)
        this.Output.WriteLine(" ]")

        for i = 0 to search_results.Length - 1 do
            if i = selected then
                this.Output.Write(" > ")
                this.Output.Button(display search_results.[i], "ok", Brushes.Yellow)
                this.Output.WriteLine(" <")
            else
                let offset = (search_results.Length + i - selected) % search_results.Length
                this.Output.Button(sprintf "%02i %s" offset (display search_results.[i]), sprintf "%i" offset, Brushes.LightGray)
                this.Output.WriteLine()

        this.Output.WriteLine()
        this.Output.Button(" ok ", "ok", Brushes.LightGray, Brush.Parse("#101010"))
        this.Output.Write(" ")
        this.Output.Button(" back ", "back", Brushes.LightGray, Brush.Parse("#101010"))
        this.Output.Write(" ")
        this.Output.Button(" new ", "new", Brushes.LightGray, Brush.Parse("#101010"))
        this.Output.Write(" ")
        this.Output.Button(" delete ", "delete", Brushes.LightGray, Brush.Parse("#101010"))

    override this.Next(user_input: string) : bool =
        if submenu.HasMenu then
            if not (submenu.Next(user_input)) then
                add current_editor
                refresh_search()
                this.Draw()
            true
        else

        match user_input with
        | "back" ->
            save()
            false
        | "ok" ->
            current_editor <- search_results.[selected]
            remove current_editor
            submenu.Open(edit (fun () -> current_editor) (fun e -> current_editor <- e))
            true
        | "new" ->
            submenu.Open(create (fun v -> current_editor <- v))
            true
        | "delete" ->
            remove search_results.[selected]
            refresh_search()
            this.Draw()
            true
        | _ ->
            match Int32.TryParse(user_input) with
            | true, n ->
                selected <- ((selected + n) % search_results.Length + search_results.Length) % search_results.Length
            | false, _ ->
                search_query <- user_input
                refresh_search()
            this.Draw()
            true

    override this.Start() : bool = this.Draw(); true
namespace Loana.Database

open Loana

type Nouns(path) =

    let file = NounFile(path)
    let nouns: ResizeArray<Noun> = ResizeArray(file.Read())

    member this.Add = nouns.Add
    member this.AddRange = nouns.AddRange
    member this.Clear = nouns.Clear
    member this.Remove = nouns.Remove

    member this.Search (text: string) =
        seq {
            for noun in nouns do
                if
                    noun.Deutsch.Contains(text, System.StringComparison.OrdinalIgnoreCase)
                    || noun.English.Text.Contains(text, System.StringComparison.OrdinalIgnoreCase)
                    || noun.EnglishAlternatives |> List.exists (fun s -> s.Text.Contains(text, System.StringComparison.OrdinalIgnoreCase))
                then
                    yield noun
        }

    member this.Save() =
        file.Write(nouns.ToArray())

    member this.Validate(output: IOutput) : unit =
        let mutable seen_keys = Set.empty
        let mutable seen_definitions = Map.empty
        for noun in nouns do
            if noun.English.Text.Length = 0 then
                output.WriteLine(sprintf "%O: english definition missing" noun)

            elif noun.English.Text.Trim() <> noun.English.Text then
                output.WriteLine(sprintf "%O: english definition must be trimmed of whitespace" noun)

            if noun.Deutsch.Length = 0 then
                output.WriteLine(sprintf "%O: german definition missing" noun)

            elif not (System.Char.IsUpper noun.Deutsch.[0]) then
                output.WriteLine(sprintf "%O: german definition must be uppercase" noun)

            elif noun.Deutsch.Trim() <> noun.Deutsch then
                output.WriteLine(sprintf "%O: german definition must be trimmed of whitespace" noun)

            if seen_keys.Contains(noun.ToString()) then
                printfn "%O: duplicate detected" noun
            else
                seen_keys <- seen_keys.Add(noun.ToString())

            let definitions = Key.of_german noun.English.Text + "-" + String.concat "-" (noun.EnglishAlternatives |> Seq.map _.Text |> Seq.map Key.of_german)
            if seen_definitions.ContainsKey(definitions) then
                printfn "%O: duplicate or ambiguous english definitions detected with %O" noun seen_definitions.[definitions]
            else
                seen_definitions <- seen_definitions.Add(definitions, noun)

        for noun in nouns do
            match noun.Guts with
            | Masculine plural
            | Feminine plural
            | Neuter plural ->
                match plural with
                | Something plural_form ->
                    let plural_key = "p_" + Key.of_german plural_form.Deutsch
                    if seen_keys.Contains plural_key then
                        output.WriteLine(sprintf "%O: duplicate plural-only form '%s'" noun plural_form.Deutsch)
                | _ -> ()
            | Plural -> ()

open Avalonia.Media
open Loana.Interface

type CreateNounMenu(save: Noun -> unit, output: IOutput) =
    inherit Menu(output)

    let mutable step = 0
    let mutable de = ""
    let mutable en = ""
    let mutable en_alts = []
    let mutable gender: Gender = Gender.Masculine
    let mutable plural_de = Nothing
    let mutable plural_en = ""
    let mutable plural_en_alts = []

    let display_step() =
        match step with
        | 0 -> output.Write("Deutsch: ")
        | 1 -> output.Write("English: ")
        | 2 -> output.Write("Gender: ")
        | 3 -> output.Write("Deutsch Plural?: ")
        | 4 -> output.Write("English Plural: ")
        | _ -> failwith "impossible"

    override this.Start() : bool =
        output.Clear()
        display_step()
        true

    override this.Next(user_input: string) : bool =
        let user_input = user_input.Trim()
        match step with
        | 0 ->
            if user_input.Length > 0 && System.Char.IsUpper(user_input.[0]) then
                de <- user_input
                step <- 1
                output.WriteLine(user_input, Brushes.Green)
            else
                output.WriteLine(user_input, Brushes.Red)
        | 1 ->
            let split = user_input.Split(",", System.StringSplitOptions.RemoveEmptyEntries ||| System.StringSplitOptions.TrimEntries)
            if split.Length > 0 then
                en <- split.[0]
                en_alts <- split |> Seq.skip 1 |> List.ofSeq
                step <- 2
                output.WriteLine(user_input, Brushes.Green)
            else
                output.WriteLine(user_input, Brushes.Red)
        | 2 ->
            match user_input with
            | "m" | "f" | "n" | "p" ->
                gender <- Gender.Parse user_input
                step <- if gender = Gender.Plural then 5 else 3
                output.WriteLine(user_input, Brushes.Green)
            | _ ->
                output.WriteLine(user_input, Brushes.Red)
        | 3 ->
            match user_input with
            | "" ->
                plural_de <- ToBeDetermined
                step <- 5
            | "n/a" ->
                plural_de <- Nothing
                step <- 5
                output.WriteLine(user_input, Brushes.Yellow)
            | _ when System.Char.IsUpper(user_input.[0]) ->
                plural_de <- Something user_input
                step <- 4
                output.WriteLine(user_input, Brushes.Green)
            | _ ->
                output.WriteLine(user_input, Brushes.Red)
        | 4 ->
            let split = user_input.Split(",", System.StringSplitOptions.RemoveEmptyEntries ||| System.StringSplitOptions.TrimEntries)
            if split.Length > 0 then
                plural_en <- split.[0]
                plural_en_alts <- split |> Seq.skip 1 |> List.ofSeq
                step <- 5
                output.WriteLine(user_input, Brushes.Green)
            else
                output.WriteLine(user_input, Brushes.Red)
        | _ -> failwith "impossible"

        if step = 5 then
            let plural = match plural_de with Nothing -> Nothing | ToBeDetermined -> ToBeDetermined | Something x -> Something { Deutsch = x; English = Annotation.Parse plural_en; EnglishAlternatives = List.map Annotation.Parse plural_en_alts }
            save {
                Translation = { Deutsch = de; English = Annotation.Parse en; EnglishAlternatives = List.map Annotation.Parse en_alts }
                Guts =
                    match gender with
                    | Gender.Masculine -> Masculine plural
                    | Gender.Feminine -> Feminine plural
                    | Gender.Neuter -> Neuter plural
                    | Gender.Plural -> Plural
            }
            false
        else
            display_step()
            true

module NounBrowser =

    let create (nouns: Nouns, output: IOutput) =

        let edit_plural get set =
            EditorMenu(
                [|
                    {
                        Name = "Deutsch"
                        Draw = fun (p: Vocab) (output: IOutput) -> output.Write p.Deutsch
                        Menu = fun get set ->
                            EditTextFieldMenu(
                                (fun () -> get().Deutsch),
                                (fun t -> let g = get() in set({ g with Deutsch = t })),
                                output
                            )
                    }
                    {
                        Name = "English"
                        Draw = fun (p: Vocab) (output: IOutput) -> output.Write p.English.Text
                        Menu = fun get set ->
                            EditTextFieldMenu(
                                (fun () -> get().English.Text),
                                (fun t -> let g = get() in set({ g with English = Annotation.Parse t })),
                                output
                            )
                    }
                |],
                get,
                set,
                output
            ) :> Menu

        let edit get set =
            EditorMenu(
                [|
                    {
                        Name = "Deutsch"
                        Draw = fun (noun: Noun) (output: IOutput) -> output.Write noun.Deutsch
                        Menu = fun get set ->
                            EditTextFieldMenu(
                                (fun () -> get().Deutsch),
                                (fun t -> let g = get() in set({ g with Translation = { g.Translation with Deutsch = t } })),
                                output
                            )
                    }
                    {
                        Name = "English"
                        Draw = fun (noun: Noun) (output: IOutput) -> output.Write noun.English.Text
                        Menu = fun get set ->
                            EditTextFieldMenu(
                                (fun () -> get().English.Text),
                                (fun t -> let g = get() in set({ g with Translation = { g.Translation with English = Annotation.Parse t } })),
                                output
                            )
                    }
                    {
                        Name = "Gender"
                        Draw = fun (noun: Noun) (output: IOutput) -> output.Write(noun.Guts.Gender.ToString(), noun.Guts.Gender.Color)
                        Menu = fun get set ->
                            EditTextFieldMenu(
                                (fun () -> get().Guts.Gender.ToString()),
                                (fun t ->
                                    let current = get()
                                    match current.Guts with
                                    | Masculine s
                                    | Feminine s
                                    | Neuter s ->
                                        match t with
                                        | "m" -> set({ current with Guts = Masculine s })
                                        | "f" -> set({ current with Guts = Feminine s })
                                        | "n" -> set({ current with Guts = Neuter s })
                                        | _ -> ()
                                    | Plural -> ()
                                ),
                                output
                            )
                    }
                    {
                        Name = "Plural"
                        Draw = fun (noun: Noun) (output: IOutput) ->
                            match noun.Guts with
                            | Masculine plural
                            | Feminine plural
                            | Neuter plural ->
                                match plural with
                                | Nothing -> output.Write("N/A", Brushes.LightGray)
                                | ToBeDetermined -> output.Write("???", Brushes.Yellow)
                                | Something x -> output.Write x.Deutsch
                            | Plural -> output.Write("--", Brushes.LightGray)
                        Menu = fun get set ->
                            if get().Guts.IsPlural then DummyMenu(output) else
                            edit_plural
                                (fun () ->
                                    match get().Guts with
                                    | Masculine p
                                    | Feminine p
                                    | Neuter p ->
                                        match p with
                                        | Something a -> a
                                        | _ -> { Deutsch = ""; English = { Text = ""; Note = None }; EnglishAlternatives = [] }
                                    | _ -> failwith "impossible"
                                )
                                (fun x ->
                                    let x2 = if x.Deutsch <> "" then Something x else Nothing
                                    let g = get()
                                    set({ g with Guts = match g.Guts with Masculine _ -> Masculine x2 | Feminine _ -> Feminine x2 | Neuter p -> Neuter x2 | Plural -> Plural}))

                    }
                |],
                get,
                set,
                output
            ) :> Menu

        BrowserMenu(
            nouns.Search,
            (fun (noun: Noun) -> sprintf "%s [%O]" noun.Deutsch noun.Guts.Gender),
            (fun callback -> CreateNounMenu(callback, output)),
            nouns.Remove >> ignore,
            nouns.Add,
            edit,
            nouns.Save,
            output
        )
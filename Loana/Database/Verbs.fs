namespace Loana.Database

open Loana

type Verbs(path) =

    let file = VerbFile(path)
    let verbs: ResizeArray<Verb> = ResizeArray(file.Read())

    member this.Add = verbs.Add
    member this.AddRange = verbs.AddRange
    member this.Clear = verbs.Clear
    member this.Remove = verbs.Remove

    member this.Search (text: string) =
        seq {
            for verb in verbs do
                if
                    verb.Deutsch.Contains(text, System.StringComparison.OrdinalIgnoreCase)
                    || verb.English.Contains(text, System.StringComparison.OrdinalIgnoreCase)
                    || verb.EnglishAlternatives |> List.exists (fun s -> s.Contains(text, System.StringComparison.OrdinalIgnoreCase))
                then
                    yield verb
        }

    member this.Save() =
        file.Write(verbs.ToArray())

    member this.Validate(output: IOutput) : unit =
        let mutable seen_keys = Set.empty
        let mutable seen_definitions = Map.empty
        for verb in verbs do
            if verb.English.Length = 0 then
                output.WriteLine(sprintf "%O: english definition missing" verb)

            elif verb.English.Trim().ToLower() <> verb.English then
                output.WriteLine(sprintf "%O: english definition must be lowercase and trimmed of whitespace" verb)

            if verb.Deutsch.Length = 0 then
                output.WriteLine(sprintf "%O: german definition missing" verb)

            elif verb.Deutsch.Trim().ToLower() <> verb.Deutsch then
                output.WriteLine(sprintf "%O: german definition must be lowercase and trimmed of whitespace" verb)

            if seen_keys.Contains(verb.ToString()) then
                printfn "%O: duplicate detected" verb
            else
                seen_keys <- seen_keys.Add(verb.ToString())

            let definitions = Key.of_german verb.English + "-" + String.concat "-" (verb.EnglishAlternatives |> Seq.map Key.of_german)
            if seen_definitions.ContainsKey(definitions) then
                printfn "%O: duplicate or ambiguous english definitions detected with %O" verb seen_definitions.[definitions]
            else
                seen_definitions <- seen_definitions.Add(definitions, verb)

open Avalonia.Media
open Loana.Interface

type CreateVerbMenu(save: Verb -> unit, output: IOutput) =
    inherit Menu(output)

    let mutable step = 0
    let mutable de = ""
    let mutable en = ""
    let mutable en_alts = []
    let mutable separable = false
    let mutable tag = VerbTag.None

    let display_step() =
        match step with
        | 0 -> output.Write("Deutsch: ")
        | 1 -> output.Write("English: ")
        | 2 -> output.Write("Separable?: ")
        | 3 -> output.Write("Transitive|Intransitive|Reflexive|Reciprocal?: ")
        | 4 -> output.Write("English Plural: ")
        | _ -> failwith "impossible"

    override this.Start() : bool =
        output.Clear()
        display_step()
        true

    override this.Next(user_input: string) : bool =
        let user_input = user_input.Trim().ToLower()
        match step with
        | 0 ->
            if user_input.Length > 0 then
                de <- user_input
                step <- 1
                output.WriteLine(user_input, Brushes.Green)
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
            separable <- user_input.Length > 0
            step <- 3
            output.WriteLine(user_input, Brushes.Green)
        | 3 ->
            match user_input with
            | "transitive" ->
                tag <- VerbTag.Transitive
                step <- 4
                output.WriteLine(user_input, Brushes.Green)
            | "intransitive" ->
                tag <- VerbTag.Intransitive
                step <- 4
                output.WriteLine(user_input, Brushes.Green)
            | "reflexive" ->
                tag <- VerbTag.Reflexive
                step <- 4
                output.WriteLine(user_input, Brushes.Green)
            | "reciprocal" ->
                tag <- VerbTag.Reciprocal
                step <- 4
                output.WriteLine(user_input, Brushes.Green)
            | "" ->
                tag <- VerbTag.None
                step <- 4
            | _ ->
                output.WriteLine(user_input, Brushes.Red)
        | _ -> failwith "impossible"

        if step = 4 then
            save {
                Infinitive = { Deutsch = de; English = en; EnglishAlternatives = en_alts }
                Separable = separable
                Tag = tag
                Inflections = Map.empty
            }
            false
        else
            display_step()
            true

module VerbBrowser =

    let create (verbs: Verbs, output: IOutput) =

        let edit get set =
            EditorMenu(
                [|
                    {
                        Name = "Deutsch"
                        Draw = fun (verb: Verb) (output: IOutput) -> output.Write verb.Deutsch
                        Menu = fun get set ->
                            EditTextFieldMenu(
                                (fun () -> get().Deutsch),
                                (fun t -> let g = get() in set({ g with Infinitive = { g.Infinitive with Deutsch = t } })),
                                output
                            )
                    }
                    {
                        Name = "English"
                        Draw = fun (verb: Verb) (output: IOutput) -> output.Write verb.English
                        Menu = fun get set ->
                            EditTextFieldMenu(
                                (fun () -> get().English),
                                (fun t -> let g = get() in set({ g with Infinitive = { g.Infinitive with English = t } })),
                                output
                            )
                    }
                    {
                        Name = "Separable"
                        Draw = fun (verb: Verb) (output: IOutput) -> if verb.Separable then output.Write("Yes", Brushes.Green) else output.Write("No", Brushes.LightGray)
                        Menu = fun get set -> DummyMenu(output)
                    }
                    {
                        Name = "Type"
                        Draw = fun (verb: Verb) (output: IOutput) -> output.Write(verb.Tag.ToString(), verb.Tag.Color)
                        Menu = fun get set -> DummyMenu(output)
                    }
                |],
                get,
                set,
                output
            ) :> Menu

        BrowserMenu(
            verbs.Search,
            (fun (verb: Verb) -> verb.Deutsch),
            (fun callback -> CreateVerbMenu(callback, output)),
            verbs.Remove >> ignore,
            verbs.Add,
            edit,
            verbs.Save,
            output
        )
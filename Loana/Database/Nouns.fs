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
                    || noun.English.Contains(text, System.StringComparison.OrdinalIgnoreCase)
                    || noun.EnglishAlternatives |> List.exists (fun s -> s.Contains(text, System.StringComparison.OrdinalIgnoreCase))
                then
                    yield noun
        }

    member this.Save() =
        file.Write(nouns.ToArray())

    member this.Validate(output: IOutput) : unit =
        let mutable seen_keys = Set.empty
        let mutable seen_definitions = Map.empty
        for noun in nouns do
            if noun.English.Length = 0 then
                output.WriteLine(sprintf "%O: english definition missing" noun)

            elif noun.English.Trim() <> noun.English then
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

            let definitions = Key.of_german noun.English + "-" + String.concat "-" (noun.EnglishAlternatives |> Seq.map Key.of_german)
            if seen_definitions.ContainsKey(definitions) then
                printfn "%O: duplicate or ambiguous english definitions detected with %O" noun seen_definitions.[definitions]
            else
                seen_definitions <- seen_definitions.Add(definitions, noun)

        for noun in nouns do
            match noun.Guts with
            | Masculine s
            | Feminine s
            | Neuter s ->
                match s.Plural with
                | Something plural_form ->
                    let plural_key = "p_" + Key.of_german plural_form
                    if not (seen_keys.Contains plural_key) then
                        output.WriteLine(sprintf "%O: plural form '%s' not in database" noun plural_form)
                | _ -> ()
            | Plural p ->
                match p.Singular with
                | Something singular ->
                    let singular_key = Key.of_german singular
                    if not (seen_keys.Contains ("m_" + singular_key) || seen_keys.Contains ("f_" + singular_key) || seen_keys.Contains ("n_" + singular_key)) then
                        output.WriteLine(sprintf "%O: singular form '%s' not in database" noun singular)
                | _ -> ()

open Loana.Interface

module NounBrowser =

    let create (nouns: Nouns, output: IOutput) =

        let edit get set =
            EditorMenu(
                [|
                    {
                        Name = "Deutsch"
                        Draw = fun (noun: Noun) (output: IOutput) -> output.Write noun.Deutsch
                        Menu = fun get set ->
                            EditTextFieldMenu(
                                (fun () -> get().Deutsch),
                                (fun t -> set({ get() with Deutsch = t })),
                                output
                            )
                    }
                    {
                        Name = "English"
                        Draw = fun (noun: Noun) (output: IOutput) -> output.Write noun.English
                        Menu = fun get set ->
                            EditTextFieldMenu(
                                (fun () -> get().English),
                                (fun t -> set({ get() with English = t })),
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
                                    | Plural _ -> ()
                                ),
                                output
                            )
                    }
                |],
                get,
                set,
                output
            ) :> Menu

        let create () =
            {
                Deutsch = ""
                English = ""
                EnglishAlternatives = []
                Guts = Masculine { Plural = ToBeDetermined }
            }

        BrowserMenu(
            nouns.Search,
            (fun (noun: Noun) -> sprintf "%s [%O]" noun.Deutsch noun.Guts.Gender),
            create,
            nouns.Remove >> ignore,
            nouns.Add,
            edit,
            output
        )
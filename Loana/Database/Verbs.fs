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

module VerbDownloader =

    open System
    open System.Text.RegularExpressions
    open System.Net.Http

    let http_client = new HttpClient()

    let private download_de_verb_page(verb: string) =
        http_client.GetStringAsync(new Uri(sprintf "https://conjugator.reverso.net/conjugation-german-verb-%s.html" (Uri.EscapeDataString verb)))
        |> Async.AwaitTask
        |> Async.RunSynchronously

    let private download_en_verb_page(verb: string) =
        http_client.GetStringAsync(new Uri(sprintf "https://conjugator.reverso.net/conjugation-english-verb-%s.html" (Uri.EscapeDataString verb)))
        |> Async.AwaitTask
        |> Async.RunSynchronously

    // yeah, yeah...
    let private find_conjugation_list (title: string) (html: string) : Map<string, string> =
        let list = Regex.Match(html, sprintf "<div class=\"blue-box-wrap\" mobile-title=\"%s\">(.*?)<\/div>" title).Groups.[1].Value

        let items = Regex.Matches(list, "<li>(.*?)<\/li>")
        seq {
            for li in items do
                let text = li.Groups.[1].Value
                let gray = Regex.Match(text, "<i class=\"graytxt\">(.*?)<\/i>").Groups.[1].Value
                let rest = Regex.Matches(text, "<i class=\".*?e.*?txt\">(.*?)<\/i>") |> Seq.map (fun m -> m.Groups.[1].Value) |> String.concat ""

                yield gray.Trim(), rest.Trim()
        }
        |> Map.ofSeq

    let private find_participle (title: string) (html: string) : string =
        let list = Regex.Match(html, sprintf "<div class=\"blue-box-wrap.*?\" mobile-title=\"%s\\s*\">(.*?)<\/div>" title).Groups.[1].Value

        Regex.Match(list, "<i class=\"particletxt\">(.*?)<\/i>").Groups.[1].Value +
        Regex.Match(list, "<i class=\"verbtxt\">(.*?)<\/i>").Groups.[1].Value

    let extend_verb(verb: Verb, output: IOutput) : Verb =
        output.WriteLine("Downloading HTML ...")
        let de_html = download_de_verb_page(Key.of_german verb.Infinitive.Deutsch)
        let en_html = download_en_verb_page(verb.Infinitive.English)
        output.WriteLine("Parsing HTML ...")
        let de_present_tense = find_conjugation_list "Indikativ Präsens" de_html
        let en_present_tense = find_conjugation_list "Indicative Present" en_html
        let de_past_participle = find_participle "Partizip Perfekt" de_html
        let en_past_participle = find_participle "Participle Past" en_html
        let en_infinitive = (find_participle "Infinitive" en_html).Replace("to ", "").Trim()

        verb
            .WithInflection(Present FirstSingular, de_present_tense.["ich"], verb.Infinitive.English.Replace(en_infinitive, en_present_tense.["I"]))
            .WithInflection(Present FirstThirdPluralFormal, de_present_tense.["Sie"], verb.Infinitive.English.Replace(en_infinitive, en_present_tense.["we"]))
            .WithInflection(Present SecondSingular, de_present_tense.["du"], verb.Infinitive.English.Replace(en_infinitive, en_present_tense.["you"]))
            .WithInflection(Present SecondPlural, de_present_tense.["ihr"], verb.Infinitive.English.Replace(en_infinitive, en_present_tense.["you"]))
            .WithInflection(Present ThirdSingular, de_present_tense.["er/sie/es"], verb.Infinitive.English.Replace(en_infinitive, en_present_tense.["he/she/it"]))
            .WithInflection(PastParticiple, de_past_participle, verb.Infinitive.English.Replace(en_infinitive, en_past_participle))

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
            separable <- user_input.Length > 0 && not (user_input.Contains "n")
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
            | "reflexive" when de.StartsWith "sich " ->
                tag <- VerbTag.Reflexive
                step <- 4
                output.WriteLine(user_input, Brushes.Green)
            | "reciprocal" when de.StartsWith "sich " ->
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
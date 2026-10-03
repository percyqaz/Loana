namespace Loana.Language

open System
open System.Text.RegularExpressions
open System.Net.Http

module VerbDownloader =

    let http_client = new HttpClient()

    let private download_de_verb_page (verb: string) : string =
        http_client.GetStringAsync(
            Uri(sprintf "https://conjugator.reverso.net/conjugation-german-verb-%s.html" (Uri.EscapeDataString(verb)))
        )
        |> Async.AwaitTask
        |> Async.RunSynchronously

    let private download_en_verb_page (verb: string) : string =
        http_client.GetStringAsync(
            Uri(sprintf "https://conjugator.reverso.net/conjugation-english-verb-%s.html" (Uri.EscapeDataString(verb)))
        )
        |> Async.AwaitTask
        |> Async.RunSynchronously

    // yeah, yeah...
    let private find_conjugation_list (title: string) (html: string) : Map<string, string> =
        let div_matching_title =
            Regex
                .Match(html, sprintf "<div class=\"blue-box-wrap\" mobile-title=\"%s\">(.*?)<\/div>" title)
                .Groups.[1].Value

        let li_elements = Regex.Matches(div_matching_title, "<li>(.*?)<\/li>")

        seq {
            for li in li_elements do
                let text = li.Groups.[1].Value
                let gray = Regex.Match(text, "<i class=\"graytxt\">(.*?)<\/i>").Groups.[1].Value

                let rest =
                    Regex.Matches(text, "<i class=\".*?e.*?txt\">(.*?)<\/i>")
                    |> Seq.map _.Groups.[1].Value
                    |> String.concat ""

                yield gray.Trim(), rest.Trim()
        }
        |> Map.ofSeq

    let private find_participle (title: string) (html: string) : string =
        let div_matching_title =
            Regex
                .Match(html, sprintf "<div class=\"blue-box-wrap.*?\" mobile-title=\"%s\\s*\">(.*?)<\/div>" title)
                .Groups.[1].Value

        Regex.Match(div_matching_title, "<i class=\"particletxt\">(.*?)<\/i>").Groups.[1].Value
        + Regex.Match(div_matching_title, "<i class=\"verbtxt\">(.*?)<\/i>").Groups.[1].Value
        + Regex.Match(div_matching_title, "<i class=\"auxgraytxt\">(.*?)<\/i>").Groups.[1].Value

    let fetch_verb_inflections (verb: Verb) : (VerbInflection * string) seq =

        let verb_base =
            verb.Infinitive.Deutsch.Replace("+D", "").Replace("+A", "").Replace("+N", "").Replace("+G", "")

        let has_reflexive_pronoun, verb_base =
            if verb_base.StartsWith("sich ") then true, verb_base.Substring(5) else false, verb_base

        let verb_objects, verb_base =
            let words =
                verb_base.Split(' ', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

            if words.Length > 1 then
                Array.truncate (words.Length - 1) words |> String.concat " " |> Some, words.[words.Length - 1]
            else
                None, words.[words.Length - 1]

        let separated_particle, verb_base =
            if verb_base.Contains(".") then
                let s = verb_base.Split(".", 2)
                Some s.[0], s.[1]
            else
                None, verb_base

        Console.WriteLine("Downloading HTML ...")
        let de_html = download_de_verb_page(AsciiIdentifier.from_deutsch verb_base)
        Console.WriteLine("Parsing HTML ...")
        let de_present_tense = find_conjugation_list "Indikativ Präsens" de_html
        let de_past_tense = find_conjugation_list "Indikativ Präteritum" de_html
        //let de_past_participle = find_participle "Partizip Perfekt" de_html
        //let de_past_participle_aux = find_participle "Infinitiv Perfekt" de_html

        let de (pronoun_to_inflection: Map<string, string>) (person: Person) =
            let pronoun =
                match InflectionPerson.FromPerson(person) with
                | FirstSingular -> "ich"
                | FirstThirdPluralFormal -> "Sie"
                | ThirdSingular -> "er/sie/es"
                | SecondSingular -> "du"
                | SecondPlural -> "ihr"

            pronoun_to_inflection.[pronoun]
            + (if has_reflexive_pronoun then
                   " " + AnnotationTree.flatten_tree(Deutsch.reflexive_pronoun person verb.Dative)
               else
                   "")
            + (
                match verb_objects with
                | Some objects -> " " + objects
                | None -> ""
            )
            + (
                match separated_particle with
                | Some x -> " " + x
                | None -> ""
            )

        seq {
            for q in verb.Tenses do
                match q with
                | VerbTense.Present ->
                    for person in Person.LIST do
                        yield VerbInflection.Present(InflectionPerson.FromPerson(person)), de de_present_tense person
                | VerbTense.SimplePast ->
                    for person in Person.LIST do
                        yield VerbInflection.SimplePast(InflectionPerson.FromPerson(person)), de de_past_tense person
                | VerbTense.Imperative -> failwith "nyi"
        }

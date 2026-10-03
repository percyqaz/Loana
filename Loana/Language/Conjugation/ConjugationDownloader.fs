namespace Loana.Language

open System
open System.Text.RegularExpressions
open System.Net.Http

module ConjugationDownloader =

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

    let private make_tense_map (map: Map<string, string>) : ConjugationTense =
        Map.ofArray
            [|
                FirstSingular, map.["ich"]
                FirstPlural, map.["Sie"]
                ThirdPlural_Formal, map.["Sie"]
                ThirdSingular, map.["er/sie/es"]
                SecondSingular, map.["du"]
                SecondPlural, map.["ihr"]
            |]

    let fetch_conjugations (verb: ParsedVerb) : ConjugationContext =

        Console.WriteLine("Downloading HTML ...")
        let de_html = download_de_verb_page(AsciiIdentifier.from_deutsch verb.Base)
        Console.WriteLine("Parsing HTML ...")
        let de_present_tense = find_conjugation_list "Indikativ Präsens" de_html
        let de_past_tense = find_conjugation_list "Indikativ Präteritum" de_html
        //let de_past_participle = find_participle "Partizip Perfekt" de_html
        //let de_past_participle_aux = find_participle "Infinitiv Perfekt" de_html

        {
            ParsedVerb = verb
            Present = make_tense_map(de_present_tense)
            SimplePast = make_tense_map(de_past_tense)
            Imperative = Map.empty
        }

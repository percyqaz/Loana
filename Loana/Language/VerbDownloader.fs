namespace Loana.Language

open System
open System.Text.RegularExpressions
open System.Net.Http
open Loana.CLI

module VerbDownloader =

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

    let extend_verb(verb: Verb) : Verb =
        Console.WriteLine("Downloading HTML ...")
        let en_without_to = verb.Infinitive.English.Text.Substring(3)
        let de_html = download_de_verb_page(Key.of_german verb.Infinitive.Deutsch)
        let en_html = download_en_verb_page(en_without_to)
        Console.WriteLine("Parsing HTML ...")
        let de_present_tense = find_conjugation_list "Indikativ Präsens" de_html
        let en_present_tense = find_conjugation_list "Indicative Present" en_html
        let de_past_participle = find_participle "Partizip Perfekt" de_html
        let en_past_participle = find_participle "Participle Past" en_html
        let en_infinitive = (find_participle "Infinitive" en_html).Replace("to ", "").Trim()

        verb
            .WithInflection(Present FirstSingular, de_present_tense.["ich"], en_without_to.Replace(en_infinitive, en_present_tense.["I"]))
            .WithInflection(Present FirstThirdPluralFormal, de_present_tense.["Sie"], en_without_to.Replace(en_infinitive, en_present_tense.["we"]))
            .WithInflection(Present SecondSingular, de_present_tense.["du"], en_without_to.Replace(en_infinitive, en_present_tense.["you"]))
            .WithInflection(Present SecondPlural, de_present_tense.["ihr"], en_without_to.Replace(en_infinitive, en_present_tense.["you"]))
            .WithInflection(Present ThirdSingular, de_present_tense.["er/sie/es"], en_without_to.Replace(en_infinitive, en_present_tense.["he/she/it"]))
            .WithInflection(PastParticiple, de_past_participle, en_without_to.Replace(en_infinitive, en_past_participle))
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

        let verb_base = verb.Infinitive.Deutsch
        let sich, verb_base =
            if verb_base.StartsWith("sich ") then
                true, verb_base.Substring(5)
            else
                false, verb_base
        // todo: other words lying around before verb
        let separation, verb_base =
            if verb_base.Contains(".") then
                let s = verb_base.Split(".", 2)
                Some s.[0], s.[1]
            else
                None, verb_base
        // todo?: prepositions after

        let en_without_to = verb.Infinitive.English.Text.Substring(3)
        let de_html = download_de_verb_page(Key.of_german verb_base)
        let en_html = download_en_verb_page(en_without_to)
        Console.WriteLine("Parsing HTML ...")
        let de_present_tense = find_conjugation_list "Indikativ Präsens" de_html
        let en_present_tense = find_conjugation_list "Indicative Present" en_html
        let de_past_participle = find_participle "Partizip Perfekt" de_html
        let en_past_participle = find_participle "Participle Past" en_html
        let en_infinitive = (find_participle "Infinitive" en_html).Replace("to ", "").Trim()

        printfn "%A %A %A" sich separation verb_base

        let de (m: Map<string, string>) (person: Person) =
            let key =
                match VerbPerson.OfPerson person with
                | FirstSingular -> "ich"
                | FirstThirdPluralFormal -> "Sie"
                | ThirdSingular -> "er/sie/es"
                | SecondSingular -> "du"
                | SecondPlural -> "ihr"
            AnnotationTree.flatten_tree(Deutsch.personal_pronoun person Case.Nominative)
            + " " + m.[key]
            + (if sich then " " + AnnotationTree.flatten_tree(Deutsch.reflexive_pronoun person false) else "") // ideally verbs could specify if they are dative verbs
            + (match separation with Some s -> " " + s | None -> "")

        printfn "%A" de_present_tense
        printfn "%A" en_present_tense

        verb
            .WithInflection({ Deutsch = de de_present_tense (Person.First false); English = Annotation.Parse $"""I {en_without_to.Replace(en_infinitive, en_present_tense.["I"])}"""; EnglishAlternatives = [] })
            .WithInflection({ Deutsch = de de_present_tense (Person.Second false); English = Annotation.Parse $"""you {en_without_to.Replace(en_infinitive, en_present_tense.["you"])}"""; EnglishAlternatives = [] })
            .WithInflection({ Deutsch = de de_present_tense (Person.Third Gender.Masculine); English = Annotation.Parse $"""he {en_without_to.Replace(en_infinitive, en_present_tense.["he/she/it"])}"""; EnglishAlternatives = [] })
            .WithInflection({ Deutsch = de de_present_tense (Person.Third Gender.Feminine); English = Annotation.Parse $"""she {en_without_to.Replace(en_infinitive, en_present_tense.["he/she/it"])}"""; EnglishAlternatives = [] })
            .WithInflection({ Deutsch = de de_present_tense (Person.Third Gender.Neuter); English = Annotation.Parse $"""it {en_without_to.Replace(en_infinitive, en_present_tense.["he/she/it"])}"""; EnglishAlternatives = [] })
            .WithInflection({ Deutsch = de de_present_tense (Person.First true); English = Annotation.Parse $"""we {en_without_to.Replace(en_infinitive, en_present_tense.["we"])}"""; EnglishAlternatives = [] })
            .WithInflection({ Deutsch = de de_present_tense (Person.Second true); English = Annotation.Parse $"""you {en_without_to.Replace(en_infinitive, en_present_tense.["you"])} [plural]"""; EnglishAlternatives = [] })
            .WithInflection({ Deutsch = de de_present_tense (Person.Third Gender.Plural); English = Annotation.Parse $"""they {en_without_to.Replace(en_infinitive, en_present_tense.["they"])}"""; EnglishAlternatives = [] })
            .WithInflection({ Deutsch = de de_present_tense Person.Formal; English = Annotation.Parse $"""you {en_without_to.Replace(en_infinitive, en_present_tense.["you"])} [formal]"""; EnglishAlternatives = [] })
            .WithInflection({ Deutsch = (Option.defaultValue "" separation) + de_past_participle; English = Annotation.Parse (en_without_to.Replace(en_infinitive, en_past_participle)); EnglishAlternatives = [] })
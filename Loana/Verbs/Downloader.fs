namespace Loana.Verbs

open System
open System.Text.RegularExpressions
open System.Net.Http
open Loana.CLI
open Loana.Language

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
        Regex.Match(list, "<i class=\"verbtxt\">(.*?)<\/i>").Groups.[1].Value +
        Regex.Match(list, "<i class=\"auxgraytxt\">(.*?)<\/i>").Groups.[1].Value
        
    let fetch_verb_inflections (verb: Verb) : (VerbInflection * string) seq =
        
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
        
        Console.WriteLine("Downloading HTML ...")
        let de_html = download_de_verb_page(Key.of_german verb_base)
        Console.WriteLine("Parsing HTML ...")
        let de_present_tense = find_conjugation_list "Indikativ Präsens" de_html
        let de_past_tense = find_conjugation_list "Indikativ Präteritum" de_html
        let de_past_participle = find_participle "Partizip Perfekt" de_html
        let de_past_participle_aux = find_participle "Infinitiv Perfekt" de_html
        
        let de (m: Map<string, string>) (person: Person) =
            let key =
                match TensePerson.OfPerson person with
                | FirstSingular -> "ich"
                | FirstThirdPluralFormal -> "Sie"
                | ThirdSingular -> "er/sie/es"
                | SecondSingular -> "du"
                | SecondPlural -> "ihr"
            m.[key]
            + (if sich then " " + AnnotationTree.flatten_tree(Deutsch.reflexive_pronoun person verb.Dative) else "")
            + (match separation with Some s -> " " + s | None -> "")
        
        seq {
            for q in verb.Quizzes do
                match q with
                | Present ->
                    for person in Person.LIST do yield VerbInflection.Present (TensePerson.OfPerson person), de de_present_tense person
                | SimplePast -> for person in Person.LIST do yield VerbInflection.SimplePast (TensePerson.OfPerson person), de de_past_tense person
                | Imperative -> failwith "nyi"
        }
        
    // let extend_verb_legacy(verb: Verb) : Verb =
    //     Console.WriteLine("Downloading HTML ...")
    //
    //     let verb_base = verb.Infinitive.Deutsch
    //     let sich, verb_base =
    //         if verb_base.StartsWith("sich ") then
    //             true, verb_base.Substring(5)
    //         else
    //             false, verb_base
    //     // todo: other words lying around before verb
    //     let separation, verb_base =
    //         if verb_base.Contains(".") then
    //             let s = verb_base.Split(".", 2)
    //             Some s.[0], s.[1]
    //         else
    //             None, verb_base
    //     // todo?: prepositions after
    //
    //     let en_without_to = verb.Infinitive.English.Text.Substring(3)
    //     let de_html = download_de_verb_page(Key.of_german verb_base)
    //     let en_html = download_en_verb_page(en_without_to)
    //     Console.WriteLine("Parsing HTML ...")
    //     let de_present_tense = find_conjugation_list "Indikativ Präsens" de_html
    //     let de_past_tense = find_conjugation_list "Indikativ Präteritum" de_html
    //     let en_present_tense = find_conjugation_list "Indicative Present" en_html
    //     let en_past_tense = find_conjugation_list "Indicative Preterite" en_html
    //     let de_past_participle = find_participle "Partizip Perfekt" de_html
    //     let de_past_participle_aux = find_participle "Infinitiv Perfekt" de_html
    //     let en_past_participle = find_participle "Participle Past" en_html
    //     let en_infinitive = (find_participle "Infinitive" en_html).Replace("to ", "").Trim()
    //
    //     let de (m: Map<string, string>) (person: Person) =
    //         let key =
    //             match TensePerson.OfPerson person with
    //             | FirstSingular -> "ich"
    //             | FirstThirdPluralFormal -> "Sie"
    //             | ThirdSingular -> "er/sie/es"
    //             | SecondSingular -> "du"
    //             | SecondPlural -> "ihr"
    //         AnnotationTree.flatten_tree(Deutsch.personal_pronoun person Case.Nominative)
    //         + " " + m.[key]
    //         + (if sich then " " + AnnotationTree.flatten_tree(Deutsch.reflexive_pronoun person false) else "") // ideally verbs could specify if they are dative verbs
    //         + (match separation with Some s -> " " + s | None -> "")
    //
    //     let en (m: Map<string, string>) (person: Person) =
    //         let key =
    //             match TensePerson.OfPerson person with
    //             | FirstSingular -> "I"
    //             | FirstThirdPluralFormal -> "we"
    //             | ThirdSingular -> "he/she/it"
    //             | SecondSingular -> "you"
    //             | SecondPlural -> "you"
    //         AnnotationTree.flatten_tree(English.personal_pronoun person Case.Nominative)
    //         + " " + en_without_to.Replace(en_infinitive, m.[key])
    //         + (match person with Person.Second true -> " [plural]" | Person.Formal -> " [formal]" | _ -> "")
    //
    //     let present person = { Deutsch = de de_present_tense person; English = Annotation.Parse (en en_present_tense person); EnglishAlternatives = [] }
    //     let past person = { Deutsch = de de_past_tense person; English = Annotation.Parse (en en_past_tense person); EnglishAlternatives = [] }
    //
    //     verb
    //     |> List.foldBack (fun p v -> v.WithInflection(present p)) (List.rev Person.LIST)
    //     |> List.foldBack (fun p v -> v.WithInflection(past p)) (List.rev Person.LIST)
    //     |> fun v -> v.WithInflection({ Deutsch = (Option.defaultValue "" separation) + de_past_participle; English = Annotation.Parse (en_without_to.Replace(en_infinitive, en_past_participle)); EnglishAlternatives = [] })
    //     |> fun v -> v.WithInflection({ Deutsch = (Option.defaultValue "" separation) + de_past_participle_aux; English = Annotation.Parse ("to have " + en_without_to.Replace(en_infinitive, en_past_participle)); EnglishAlternatives = [] })
namespace Loana.Declension

open Loana

module English =

    let indefinite_article (following_word: string) =
        if following_word.StartsWith("a") ||
           following_word.StartsWith("e") ||
           following_word.StartsWith("i") ||
           following_word.StartsWith("o") ||
           following_word.StartsWith("u") then
            "an"
        else
            "a"

    let personal_pronoun (person: Person) (case: Case) : AnnotationTree =
        match person, case with
        | _, Case.Genitive -> failwith "Genitive does not have personal pronouns, use the possessive pronoun"

        | Person.First false, Case.Nominative -> Text "I"
        | Person.First false, Case.Accusative -> Text "me"
        | Person.First false, Case.Dative -> Text "me"

        | Person.First true, Case.Nominative -> Text "we"
        | Person.First true, Case.Accusative -> Text "us"
        | Person.First true, Case.Dative -> Text "us"

        | Person.Second false, Case.Nominative -> Text "you"
        | Person.Second false, Case.Accusative -> Text "you"
        | Person.Second false, Case.Dative -> Text "you"

        | Person.Second true, Case.Nominative -> Gender(Gender.Plural, [Text "you"])
        | Person.Second true, Case.Accusative -> Gender(Gender.Plural, [Text "you"])
        | Person.Second true, Case.Dative -> Gender(Gender.Plural, [Text "you"])

        | Person.Third Gender.Masculine, Case.Nominative -> Text "he"
        | Person.Third Gender.Masculine, Case.Accusative -> Text "him"
        | Person.Third Gender.Masculine, Case.Dative -> Text "him"
        | Person.Third Gender.Feminine, Case.Nominative -> Text "she"
        | Person.Third Gender.Feminine, Case.Accusative -> Text "her"
        | Person.Third Gender.Feminine, Case.Dative -> Text "her"
        | Person.Third Gender.Neuter, Case.Nominative -> Text "it"
        | Person.Third Gender.Neuter, Case.Accusative -> Text "it"
        | Person.Third Gender.Neuter, Case.Dative -> Text "it"

        | Person.Third Gender.Plural, Case.Nominative -> Text "they"
        | Person.Third Gender.Plural, Case.Accusative -> Text "them"
        | Person.Third Gender.Plural, Case.Dative -> Text "them"

        | Person.Formal, Case.Nominative -> Annotation("Formal", [Text "you"])
        | Person.Formal, Case.Accusative -> Annotation("Formal", [Text "you"])
        | Person.Formal, Case.Dative -> Annotation("Formal", [Text "you"])
        |> fun x -> [Case(case, [x])]

    let possessive_pronoun (person: Person) : AnnotationTree =
        match person with
        | Person.First false -> Text "my"
        | Person.First true -> Text "our"
        | Person.Second false -> Text "your"
        | Person.Second true -> Gender(Gender.Plural, [Text "your"])
        | Person.Third Gender.Masculine -> Text "his"
        | Person.Third Gender.Feminine -> Text "her"
        | Person.Third Gender.Neuter -> Text "its"
        | Person.Third Gender.Plural -> Text "their"
        | Person.Formal -> Annotation("Formal", [Text "your"])
        |> fun x -> [x]

    let definite_fragment (adjective: Adjective option) (noun: Noun) (case: Case) : AnnotationTree =
        let genitive = if case.IsGenitive then "of " else ""
        match adjective with
        | Some adjective ->
            [Case(case, [Text $"{genitive}the {adjective.English} {noun.English}"])]
        | None ->
            [Case(case, [Text $"{genitive}the {noun.English}"])]

    let indefinite_fragment (adjective: Adjective option) (noun: Noun) (case: Case) : AnnotationTree =
        let genitive = if case.IsGenitive then "of " else ""
        match adjective with
        | Some adjective ->
            [Case(case, [Text $"{genitive}{indefinite_article adjective.English} {adjective.English} {noun.English}"])]
        | None ->
            [Case(case, [Text $"{genitive}{indefinite_article noun.English} {noun.English}"])]

    let possessive_fragment (person: Person) (adjective: Adjective option) (noun: Noun) (case: Case) : AnnotationTree =
        let genitive = if case.IsGenitive then [Text "of "] else []
        match adjective with
        | Some adjective ->
            [Case(case, genitive @ possessive_pronoun person @ [Text $" {adjective.English} {noun.English}"])]
        | None ->
            [Case(case, genitive @ possessive_pronoun person @ [Text $" {noun.English}"])]
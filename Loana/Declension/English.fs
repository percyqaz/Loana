namespace Loana.Core.Declension

open Loana.Core

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
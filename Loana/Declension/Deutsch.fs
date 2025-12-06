namespace Loana.Core.Declension

open Loana.Core

module Deutsch =

    let strong_ending (gender: Gender) (case: Case) : string =
        match gender, case with
        | Gender.Masculine, Case.Nominative -> "er"
        | Gender.Masculine, Case.Accusative -> "en"
        | Gender.Masculine, Case.Dative -> "em"
        | Gender.Masculine, Case.Genitive -> "en"

        | Gender.Feminine, Case.Nominative -> "e"
        | Gender.Feminine, Case.Accusative -> "e"
        | Gender.Feminine, Case.Dative -> "er"
        | Gender.Feminine, Case.Genitive -> "er"

        | Gender.Neuter, Case.Nominative -> "es"
        | Gender.Neuter, Case.Accusative -> "es"
        | Gender.Neuter, Case.Dative -> "em"
        | Gender.Neuter, Case.Genitive -> "en"

        | Gender.Plural, Case.Nominative -> "e"
        | Gender.Plural, Case.Accusative -> "e"
        | Gender.Plural, Case.Dative -> "en"
        | Gender.Plural, Case.Genitive -> "er"

    let weak_ending (gender: Gender) (case: Case) : string =
        match gender, case with
        | Gender.Masculine, Case.Nominative -> "e"
        | Gender.Masculine, Case.Accusative -> "en"
        | Gender.Masculine, Case.Dative -> "en"
        | Gender.Masculine, Case.Genitive -> "en"

        | Gender.Feminine, Case.Nominative -> "e"
        | Gender.Feminine, Case.Accusative -> "e"
        | Gender.Feminine, Case.Dative -> "en"
        | Gender.Feminine, Case.Genitive -> "en"

        | Gender.Neuter, Case.Nominative -> "e"
        | Gender.Neuter, Case.Accusative -> "e"
        | Gender.Neuter, Case.Dative -> "en"
        | Gender.Neuter, Case.Genitive -> "en"

        | Gender.Plural, Case.Nominative -> "en"
        | Gender.Plural, Case.Accusative -> "en"
        | Gender.Plural, Case.Dative -> "en"
        | Gender.Plural, Case.Genitive -> "en"

    let indefinite_article (gender: Gender) (case: Case) : AnnotationTree * bool =
        let ending, needs_strong_declension =
            match gender, case with
            | Gender.Masculine, Case.Nominative -> "", true
            | Gender.Masculine, Case.Accusative -> "en", false
            | Gender.Masculine, Case.Dative -> "em", false
            | Gender.Masculine, Case.Genitive -> "es", false

            | Gender.Feminine, Case.Nominative -> "e", false
            | Gender.Feminine, Case.Accusative -> "e", false
            | Gender.Feminine, Case.Dative -> "er", false
            | Gender.Feminine, Case.Genitive -> "er", false

            | Gender.Neuter, Case.Nominative -> "", true
            | Gender.Neuter, Case.Accusative -> "", true
            | Gender.Neuter, Case.Dative -> "em", false
            | Gender.Neuter, Case.Genitive -> "es", false

            | Gender.Plural, _ -> failwith "This noun is plural, there is no definite article"
        if ending <> "" then
            [Gender(gender, [Text "ein"; StrongDeclension [Text ending]])], needs_strong_declension
        else
            [Gender(gender, [Text "ein"])], needs_strong_declension

    let definite_article (gender: Gender) (case: Case) : AnnotationTree =
        match gender, case with
        | Gender.Masculine, Case.Nominative -> "der"
        | Gender.Masculine, Case.Accusative -> "den"
        | Gender.Masculine, Case.Dative -> "dem"
        | Gender.Masculine, Case.Genitive -> "des"

        | Gender.Feminine, Case.Nominative -> "die"
        | Gender.Feminine, Case.Accusative -> "die"
        | Gender.Feminine, Case.Dative -> "der"
        | Gender.Feminine, Case.Genitive -> "der"

        | Gender.Neuter, Case.Nominative -> "das"
        | Gender.Neuter, Case.Accusative -> "das"
        | Gender.Neuter, Case.Dative -> "dem"
        | Gender.Neuter, Case.Genitive -> "des"

        | Gender.Plural, Case.Nominative -> "die"
        | Gender.Plural, Case.Accusative -> "die"
        | Gender.Plural, Case.Dative -> "den"
        | Gender.Plural, Case.Genitive -> "der"
        |> fun x -> [Gender(gender, [Text x])]

    let decline_noun (noun: Noun) (case: Case) : AnnotationTree =
        match noun.Guts with
        | Plural _ when case.IsDative ->
            if noun.Deutsch.EndsWith("n") then
                [Text noun.Deutsch]
            else
                [Text noun.Deutsch; Case(case, [Text "n"])]
        | Masculine _
        | Neuter _ when case.IsGenitive ->
            if noun.Deutsch.EndsWith("e") || noun.Deutsch.EndsWith("l") || noun.Deutsch.EndsWith("r") then
                [Text noun.Deutsch; Case(case, [Text "s"])]
            else
                [Text noun.Deutsch; Case(case, [Text "es"])]
        | _ -> [Text noun.Deutsch]

    let decline_adjective_strong (adjective: Adjective) (gender: Gender) (case: Case) : AnnotationTree =
        [Text adjective.Deutsch; StrongDeclension [Text (strong_ending gender case)]]

    let decline_adjective_weak (adjective: Adjective) (gender: Gender) (case: Case) : AnnotationTree =
        [Text adjective.Deutsch; WeakDeclension [Text (weak_ending gender case)]]

    let definite_fragment (adjective: Adjective option) (noun: Noun) (case: Case) : AnnotationTree =
        let the = definite_article noun.Guts.Gender case
        let f_adjective =
            match adjective with
            | Some adjective -> [Text " "] @ decline_adjective_weak adjective noun.Guts.Gender case
            | None -> []
        let f_noun = decline_noun noun case
        [Case(case, the @ f_adjective @ [Text " "] @ f_noun)]

    let indefinite_fragment (adjective: Adjective option) (noun: Noun) (case: Case) : AnnotationTree =
        let a, needs_strong_declension = indefinite_article noun.Guts.Gender case
        let f_adjective =
            match adjective with
            | Some adjective ->
                let declension = if needs_strong_declension then decline_adjective_strong else decline_adjective_weak
                [Text " "] @ declension adjective noun.Guts.Gender case
            | None -> []
        let f_noun = decline_noun noun case
        [Case(case, a @ f_adjective @ [Text " "] @ f_noun)]
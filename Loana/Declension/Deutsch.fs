namespace Loana.Core.Declension

open Loana.Core

module Deutsch =

    let strong_adjective_ending (gender: Gender) (case: Case) : string =
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

    let weak_adjective_ending (gender: Gender) (case: Case) : string =
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

    let ein_declension (gender: Gender) (case: Case) : AnnotationTree * bool =
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

            | Gender.Plural, Case.Nominative -> "e", false
            | Gender.Plural, Case.Accusative -> "e", false
            | Gender.Plural, Case.Dative -> "en", false // unverified
            | Gender.Plural, Case.Genitive -> "er", false // unverified
        if ending <> "" then
            [(if needs_strong_declension then ArticleDeclension else StrongDeclension) [Text ending]], needs_strong_declension
        else
            [], needs_strong_declension

    let indefinite_article (gender: Gender) (case: Case) : AnnotationTree * bool =
        let ending, needs_strong_declension =
            match gender, case with
            | Gender.Plural, _ -> failwith "This noun is plural, there is no definite article"
            | _ -> ein_declension gender case
        [Gender(gender, [Text "ein"] @ ending)], needs_strong_declension

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

    let personal_pronoun (person: Person) (case: Case) : AnnotationTree =
        match person, case with
        | _, Case.Genitive -> failwith "Genitive does not have personal pronouns, use the possessive pronoun"

        | Person.First false, Case.Nominative -> Text "ich"
        | Person.First false, Case.Accusative -> Text "mich"
        | Person.First false, Case.Dative -> Text "mir"

        | Person.First true, Case.Nominative -> Text "wir"
        | Person.First true, Case.Accusative -> Text "uns"
        | Person.First true, Case.Dative -> Text "uns"

        | Person.Second false, Case.Nominative -> Text "du"
        | Person.Second false, Case.Accusative -> Text "dich"
        | Person.Second false, Case.Dative -> Text "dir"

        | Person.Second true, Case.Nominative -> Text "ihr"
        | Person.Second true, Case.Accusative -> Text "euch"
        | Person.Second true, Case.Dative -> Text "euch"

        | Person.Third Gender.Masculine, Case.Nominative -> Text "er"
        | Person.Third Gender.Masculine, Case.Accusative -> Text "ihn"
        | Person.Third Gender.Masculine, Case.Dative -> Text "ihm"
        | Person.Third Gender.Feminine, Case.Nominative -> Text "sie"
        | Person.Third Gender.Feminine, Case.Accusative -> Text "sie"
        | Person.Third Gender.Feminine, Case.Dative -> Text "ihr"
        | Person.Third Gender.Neuter, Case.Nominative -> Text "es"
        | Person.Third Gender.Neuter, Case.Accusative -> Text "es"
        | Person.Third Gender.Neuter, Case.Dative -> Text "ihm"

        | Person.Third Gender.Plural, Case.Nominative -> Text "sie"
        | Person.Third Gender.Plural, Case.Accusative -> Text "sie"
        | Person.Third Gender.Plural, Case.Dative -> Text "ihnen"

        | Person.Formal, Case.Nominative -> Text "Sie"
        | Person.Formal, Case.Accusative -> Text "Sie"
        | Person.Formal, Case.Dative -> Text "Ihnen"
        |> fun x -> [Case(case, [x])]

    let possessive_pronoun (person: Person) (gender: Gender) (case: Case) : AnnotationTree * bool =
        let ending, needs_strong_declension = ein_declension gender case
        let stem =
            match person with
            | Person.First false -> "mein"
            | Person.First true -> "unser"
            | Person.Second false -> "dein"
            | Person.Second true -> if AnnotationTree.flatten_tree ending = "e" then "eur" else "euer"
            | Person.Third Gender.Masculine -> "sein"
            | Person.Third Gender.Feminine -> "ihr"
            | Person.Third Gender.Neuter -> "sein"
            | Person.Third Gender.Plural -> "ihr"
            | Person.Formal -> "Ihr"
        [Text stem] @ ending, needs_strong_declension

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
        [Text adjective.Deutsch; StrongDeclension [Text (strong_adjective_ending gender case)]]

    let decline_adjective_weak (adjective: Adjective) (gender: Gender) (case: Case) : AnnotationTree =
        [Text adjective.Deutsch; WeakDeclension [Text (weak_adjective_ending gender case)]]

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

    let possessive_fragment (person: Person) (adjective: Adjective option) (noun: Noun) (case: Case) : AnnotationTree =
        let my, needs_strong_declension = possessive_pronoun person noun.Guts.Gender case
        let f_adjective =
            match adjective with
            | Some adjective ->
                let declension = if needs_strong_declension then decline_adjective_strong else decline_adjective_weak
                [Text " "] @ declension adjective noun.Guts.Gender case
            | None -> []
        let f_noun = decline_noun noun case
        [Case(case, my @ f_adjective @ [Text " "] @ f_noun)]
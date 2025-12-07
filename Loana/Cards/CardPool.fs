namespace Loana.Cards

open Loana.Core
open Loana.Core.Declension

module CardPool =

    let NOUNS : Noun array =
        [|
            {
                Deutsch = "Löffel"
                English = "spoon"
                Guts = Masculine { Plural = Some "Löffel" }
            }
            {
                Deutsch = "Gabel"
                English = "fork"
                Guts = Feminine { Plural = Some "Gabeln" }
            }
            {
                Deutsch = "Messer"
                English = "knife"
                Guts = Neuter { Plural = Some "Messer" }
            }
            {
                Deutsch = "Löffel"
                English = "spoons"
                Guts = Plural { Singular = Some "Löffel" }
            }
            {
                Deutsch = "Gabeln"
                English = "forks"
                Guts = Plural { Singular = Some "Gabel" }
            }
            {
                Deutsch = "Messer"
                English = "knives"
                Guts = Plural { Singular = Some "Messer" }
            }
        |]

    let ADJECTIVES : Adjective array =
        [|
            //{
            //    Deutsch = "groß"
            //    English = "big"
            //}
            {
                Deutsch = "klein"
                English = "small"
            }
        |]

    [<RequireQualifiedAccess>]
    type CardType =
        | Indefinite of Adjective option * Noun
        | Definite of Adjective option * Noun
        | Possessive of Person * Adjective option * Noun
        | Person of Person

    type CardPermutation =
        {
            Case: Case
            Type: CardType
        }
        member this.IsValid =
            match this.Type with
            | CardType.Indefinite (_, noun) when noun.Guts.IsPlural -> false
            | CardType.Person _ when this.Case.IsGenitive -> false
            | _ -> true
        member this.Generate =
            match this.Type with
            | CardType.Definite (adjective, noun) ->
                {
                    Front = English.definite_fragment adjective noun this.Case
                    Back = Deutsch.definite_fragment adjective noun this.Case
                }
            | CardType.Indefinite (adjective, noun) ->
                {
                    Front = English.indefinite_fragment adjective noun this.Case
                    Back = Deutsch.indefinite_fragment adjective noun this.Case
                }
            | CardType.Possessive (person, adjective, noun) ->
                {
                    Front = English.possessive_fragment person adjective noun this.Case
                    Back = Deutsch.possessive_fragment person adjective noun this.Case
                }
            | CardType.Person person ->
                {
                    Front = English.personal_pronoun person this.Case
                    Back = Deutsch.personal_pronoun person this.Case
                }

    let generate_card_pool () =
        seq {
            for case in [|Case.Nominative; Case.Accusative; Case.Dative; Case.Genitive|] do
                for person in [Person.First false; Person.First true; Person.Second false; Person.Second true; Person.Formal] do
                    for noun in NOUNS do
                        for adjective in ADJECTIVES do
                            yield {
                                Case = case
                                Type = CardType.Definite(Some adjective, noun)
                            }
                            yield {
                                Case = case
                                Type = CardType.Possessive(person, Some adjective, noun)
                            }
                        yield {
                            Case = case
                            Type = CardType.Definite(None, noun)
                        }
                        yield {
                            Case = case
                            Type = CardType.Possessive(person, None, noun)
                        }
                    yield {
                        Case = case
                        Type = CardType.Person person
                    }
        }
        |> Seq.filter _.IsValid
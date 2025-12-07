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
        member this.HasAdjective =
            match this with
            | Indefinite (Some _, _) -> true
            | Definite (Some _, _) -> true
            | Possessive (_, Some _, _) -> true
            | _ -> false

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
                            if person = Person.Formal then
                                yield {
                                    Case = case
                                    Type = CardType.Definite(Some adjective, noun)
                                }
                                yield {
                                    Case = case
                                    Type = CardType.Indefinite(Some adjective, noun)
                                }
                            yield {
                                Case = case
                                Type = CardType.Possessive(person, Some adjective, noun)
                            }
                        if person = Person.Formal then
                            yield {
                                Case = case
                                Type = CardType.Definite(None, noun)
                            }
                            yield {
                                Case = case
                                Type = CardType.Indefinite(None, noun)
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

    let MODES : Map<string, CardPermutation -> bool> = Map.ofList [
        "[1*] basic 'the'", fun card ->
            card.Type.IsDefinite && not card.Type.HasAdjective && card.Case.IsNominative
        "[2*] 'the'", fun card ->
            card.Type.IsDefinite && not card.Type.HasAdjective
        "[4*] 'the' + adjective", fun card ->
            card.Type.IsDefinite
        "[1*] basic 'a'", fun card ->
            card.Type.IsIndefinite && not card.Type.HasAdjective && card.Case.IsNominative
        "[2*] 'a'", fun card ->
            card.Type.IsIndefinite && not card.Type.HasAdjective
        "[4*] 'a' + adjective", fun card ->
            card.Type.IsIndefinite
        "[3*] 'a' + 'the' combo", fun card ->
            (card.Type.IsDefinite || card.Type.IsIndefinite) && not card.Type.HasAdjective
        "[5*] 'a' + 'the' adjective combo", fun card ->
            (card.Type.IsDefinite || card.Type.IsIndefinite)
        "[1*] basic personal pronouns", fun card ->
            card.Type.IsPerson && card.Case.IsNominative
        "[2*] personal pronouns", fun card ->
            card.Type.IsPerson
        "[3*] basic possessive pronouns", fun card ->
            card.Type.IsPossessive && not card.Type.HasAdjective && card.Case.IsNominative
        "[4*] possessive pronouns", fun card ->
            card.Type.IsPossessive && not card.Type.HasAdjective
        "[6*] possessive pronouns + adjective", fun card ->
            card.Type.IsPossessive
        "[4*] bit of everything, nominative", fun card ->
            (card.Type.HasAdjective || card.Type.IsPerson) && card.Case.IsNominative
        "[8*] everything", fun card ->
            card.Type.HasAdjective || card.Type.IsPerson
    ]
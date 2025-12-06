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
        | Indefinite
        | Definite

    type CardPermutation =
        {
            Noun: Noun
            Adjective: Adjective option
            Case: Case
            Type: CardType
        }
        member this.IsValid =
            // Don't try to generate "a blue houses", etc!
            not this.Noun.Guts.Gender.IsPlural || this.Type.IsDefinite
        member this.Generate =
            if this.Type.IsDefinite then
                {
                    Front = English.definite_fragment this.Adjective this.Noun this.Case
                    Back = Deutsch.definite_fragment this.Adjective this.Noun this.Case
                }
            else
                {
                    Front = English.indefinite_fragment this.Adjective this.Noun this.Case
                    Back = Deutsch.indefinite_fragment this.Adjective this.Noun this.Case
                }

    let generate_card_pool () =
        seq {
            for case in [|Case.Nominative; Case.Accusative; Case.Dative; Case.Genitive|] do
                for card_type in [|CardType.Definite; CardType.Indefinite|] do
                    for noun in NOUNS do
                        for adjective in ADJECTIVES do
                            yield { Noun = noun; Adjective = Some adjective; Case = case; Type = card_type }
                        yield { Noun = noun; Adjective = None; Case = case; Type = card_type }
        }
        |> Seq.filter _.IsValid
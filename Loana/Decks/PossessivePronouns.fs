namespace Loana.Decks

open Avalonia.Media
open Loana
open Loana.Scheduler
open Loana.Declension

type PossessivePronounsCard(person: Person, adjective: Adjective option, noun: Noun, case: Case, spacing_rule: CardSpacingRule, scheduler: CardScheduler) =
    inherit EnglishToGermanCard(
        English.possessive_fragment person adjective noun case,
        Deutsch.possessive_fragment person adjective noun case,
        $"""possessive-{person}{(if adjective.IsSome then "-adj" else "")}-{noun}-{case}""",
        spacing_rule,
        scheduler
    )

    member this.Person = person
    member this.Case = case
    member this.HasAdjective = adjective.IsSome
    member this.Gender = noun.Guts.Gender

type PossessivePronounsDeck() =
    inherit Deck<PossessivePronounsCard>()

    let spacing = CardSpacingRule.Familiarise

    override this.Name = "Possessive Pronouns"

    override this.Filters =
        [
            {
                Label = "Case"
                Filters = [
                    DeckFilter<_>.OfCase(Case.Nominative, _.Case)
                    DeckFilter<_>.OfCase(Case.Accusative, _.Case)
                    DeckFilter<_>.OfCase(Case.Dative, _.Case)
                    DeckFilter<_>.OfCase(Case.Genitive, _.Case)
                ]
            }
            {
                Label = "Gender"
                Filters = [
                    DeckFilter<_>.OfGender(Gender.Masculine, _.Gender)
                    DeckFilter<_>.OfGender(Gender.Feminine, _.Gender)
                    DeckFilter<_>.OfGender(Gender.Neuter, _.Gender)
                    DeckFilter<_>.OfGender(Gender.Plural, _.Gender)
                ]
            }
            {
                Label = "Person"
                Filters = Person.LIST |> List.map (fun p -> DeckFilter<_>.OfPerson(p, _.Person))
            }
            {
                Label = "Adjective"
                Filters = [
                    { Label = "no-adj"; Color = Brushes.Blue; Filter = _.HasAdjective >> not }
                    { Label = "adj"; Color = Brushes.Red; Filter = _.HasAdjective }
                ]
            }
        ]

    override this.Build(filters: DeckFilter<_> list list, scheduler: CardScheduler) : Card seq =
        seq {
            for person in Person.LIST do
                for noun in NOUNS do
                    for case in Case.LIST do
                        yield PossessivePronounsCard(person, Some KLEIN, noun, case, spacing, scheduler)
                        yield PossessivePronounsCard(person, None, noun, case, spacing, scheduler)
        }
        |> Seq.filter (fun card -> filters |> List.forall (fun filters -> filters |> Seq.exists (fun f -> f.Filter card)))
        |> Seq.cast
        |> Seq.cache
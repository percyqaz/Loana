namespace Loana.Decks

open Loana
open Loana.Scheduler
open Loana.Declension

type PersonalPronounsCard(person: Person, case: Case, spacing_rule: CardSpacingRule, scheduler: CardScheduler) =
    inherit EnglishToGermanCard(
        English.personal_pronoun person case,
        Deutsch.personal_pronoun person case,
        $"personal-pronoun-{person}-{case}",
        spacing_rule,
        scheduler
    )

    member this.Person = person
    member this.Case = case

type PersonalPronounsDeck() =
    inherit Deck<PersonalPronounsCard>()

    let spacing = CardSpacingRule.Familiarise

    override this.Name = "Personal Pronouns"

    override this.Filters =
        [
            {
                Label = "Case"
                Filters = [
                    DeckFilter<_>.OfCase(Case.Nominative, _.Case)
                    DeckFilter<_>.OfCase(Case.Accusative, _.Case)
                    DeckFilter<_>.OfCase(Case.Dative, _.Case)
                ]
            }
        ]

    override this.Build(filters: DeckFilter<_> list list, scheduler: CardScheduler) : Card seq =
        seq {
            for person in Person.LIST do
                for case in [Case.Nominative; Case.Accusative; Case.Dative] do
                    yield PersonalPronounsCard(person, case, spacing, scheduler)
        }
        |> Seq.filter (fun card -> filters |> List.forall (fun filters -> filters |> Seq.exists (fun f -> f.Filter card)))
        |> Seq.cast
        |> Seq.cache
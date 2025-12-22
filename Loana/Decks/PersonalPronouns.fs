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

type PersonalPronounsDeck() =
    inherit Deck()

    let spacing = CardSpacingRule.Familiarise

    override this.Name = "Personal Pronouns"
    override this.Build(scheduler: CardScheduler) : Card seq =
        seq {
            for person in Person.LIST do
                for case in [Case.Nominative; Case.Accusative; Case.Dative] do
                    yield PersonalPronounsCard(person, case, spacing, scheduler) :> Card
        } |> Seq.cache
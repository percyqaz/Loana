namespace Loana.Decks

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

type PossessivePronounsDeck() =
    inherit Deck()

    let spacing = CardSpacingRule.Familiarise

    override this.Name = "Possessive Pronouns"
    override this.Build(scheduler: CardScheduler) : Card seq =
        seq {
            for person in Person.LIST do
                for noun in NOUNS do
                    for case in Case.LIST do
                        yield PossessivePronounsCard(person, Some KLEIN, noun, case, spacing, scheduler) :> Card
                        yield PossessivePronounsCard(person, None, noun, case, spacing, scheduler)
        } |> Seq.cache
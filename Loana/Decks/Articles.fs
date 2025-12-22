namespace Loana.Decks

open Loana
open Loana.Scheduler
open Loana.Declension

type ArticlesCard(definite: bool, adjective: Adjective option, noun: Noun, case: Case, spacing_rule: CardSpacingRule, scheduler: CardScheduler) =
    inherit EnglishToGermanCard(
        (if definite then English.definite_fragment else English.indefinite_fragment) adjective noun case,
        (if definite then Deutsch.definite_fragment else Deutsch.indefinite_fragment) adjective noun case,
        $"""{(if definite then "" else "in")}definite-article-{(if adjective.IsSome then "-adj" else "")}-{noun}-{case}""",
        spacing_rule,
        scheduler
    )

    member this.IsDefinite = definite
    member this.Case = case
    member this.HasAdjective = adjective.IsSome
    member this.Gender = noun.Guts.Gender

type ArticlesDeck() =
    inherit Deck()

    let spacing = CardSpacingRule.Familiarise

    override this.Name = "Articles"
    override this.Build(scheduler: CardScheduler) : Card seq =
        seq {
            for noun in NOUNS do
                for case in Case.LIST do
                    yield ArticlesCard(true, Some KLEIN, noun, case, spacing, scheduler) :> Card
                    yield ArticlesCard(true, None, noun, case, spacing, scheduler)
                    if not noun.Guts.IsPlural then
                        yield ArticlesCard(false, Some KLEIN, noun, case, spacing, scheduler)
                        yield ArticlesCard(false, None, noun, case, spacing, scheduler)
        } |> Seq.cache
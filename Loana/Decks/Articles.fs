namespace Loana.Decks

open Avalonia.Media
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
    inherit Deck<ArticlesCard>()

    let spacing = CardSpacingRule.Familiarise

    override this.Name = "Articles"

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
                Label = "Type"
                Filters = [
                    { Label = "Definite"; Color = Brushes.Green; Filter = _.IsDefinite }
                    { Label = "Indefinite"; Color = Brushes.Yellow; Filter = _.IsDefinite >> not }
                ]
            }
            {
                Label = "With Adjective"
                Filters = [
                    { Label = "No"; Color = Brushes.Blue; Filter = _.HasAdjective >> not }
                    { Label = "Yes"; Color = Brushes.Red; Filter = _.HasAdjective }
                ]
            }
        ]

    override this.Build(filters: DeckFilter<_> list list, scheduler: CardScheduler) : Card seq =
        seq {
            for noun in NOUNS do
                for case in Case.LIST do
                    yield ArticlesCard(true, Some KLEIN, noun, case, spacing, scheduler)
                    yield ArticlesCard(true, None, noun, case, spacing, scheduler)
                    if not noun.Guts.IsPlural then
                        yield ArticlesCard(false, Some KLEIN, noun, case, spacing, scheduler)
                        yield ArticlesCard(false, None, noun, case, spacing, scheduler)
        }
        |> Seq.filter (fun card -> filters |> List.forall (fun filters -> filters |> Seq.exists (fun f -> f.Filter card)))
        |> Seq.cast
        |> Seq.cache
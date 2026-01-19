namespace Loana.Decks

open Avalonia.Media
open Loana.Interface
open Loana.Language
open Loana.Scheduler

type EnglishToGermanCard(front: AnnotationTree, back: AnnotationTree, vocab: Vocab, spacing_rule: CardSpacingRule, scheduler: CardScheduler) =
    inherit Card($"""vocab-recall-{vocab.Key}""", spacing_rule, scheduler)

    override this.DisplayFront(output: IOutput) : unit =

        AnnotationTree.render(front, output)

        output.Write(" -> English ", AnnotationTree.gradient Colors.Red Colors.Black, Brushes.White)
        output.Write(" ")
        if this.Schedule.LearningStep.IsSome then
            output.WriteLine(" Learning ", Brushes.Black, Brushes.Cyan)

    override this.DisplayBack(output: IOutput): unit =
        AnnotationTree.render(back, output)

    override this.FrontInput(user_input: string, output: IOutput) : CardEase option =
        if user_input = vocab.Deutsch then
            Some CardEase.Okay
        else
            output.WriteLine(" Mistake! See below: ", Brushes.Black, Brushes.Red)
            output.WriteLine(user_input, Brushes.LightPink)
            None

    override this.BackInput(user_input: string, output: IOutput) : CardEase = CardEase.Forgot

    static member OfNoun(noun: Noun, spacing_rule, scheduler) =
        EnglishToGermanCard([Text noun.Translation.EnglishKey], [Gender(noun.Guts.Gender, [Text noun.Deutsch])], noun.Translation, spacing_rule, scheduler)

type GermanToEnglishCard(front: AnnotationTree, back: AnnotationTree, vocab: Vocab, spacing_rule: CardSpacingRule, scheduler: CardScheduler) =
    inherit Card($"""vocab-recognise-{vocab.Key}""", spacing_rule, scheduler)

    override this.DisplayFront(output: IOutput) : unit =

        AnnotationTree.render(front, output)

        output.Write(" -> Deutsch ", Brushes.Black, AnnotationTree.gradient Colors.Yellow Colors.Red)
        output.Write(" ")
        if this.Schedule.LearningStep.IsSome then
            output.WriteLine(" Learning ", Brushes.Black, Brushes.Cyan)

    override this.DisplayBack(output: IOutput): unit =
        AnnotationTree.render(back, output)

    override this.FrontInput(user_input: string, output: IOutput) : CardEase option =
        let user_input = user_input.Trim()
        if user_input = vocab.English.Text || List.exists (fun i -> i.Text = user_input) vocab.EnglishAlternatives then
            Some CardEase.Okay
        else
            output.WriteLine(" Mistake! See below: ", Brushes.Black, Brushes.Red)
            output.WriteLine(user_input, Brushes.LightPink)
            None

    override this.BackInput(user_input: string, output: IOutput) : CardEase = CardEase.Forgot

    static member OfNoun(noun: Noun, spacing_rule, scheduler) =
        GermanToEnglishCard([Text noun.Deutsch], [Text noun.Translation.EnglishKey], noun.Translation, spacing_rule, scheduler)

type VocabDeck(word_list: Wordlist) =
    inherit Deck<Card>()

    let spacing = CardSpacingRule.Familiarise

    override this.Name = "Vocab"

    override this.Filters = []

    override this.Build(filters: DeckFilter<_> list list, scheduler: CardScheduler) : Card seq =
        seq {
            for vocab in word_list.Entries do
                match vocab.Item with
                | Noun noun ->
                    yield GermanToEnglishCard.OfNoun(noun, spacing, scheduler) :> Card
                    yield EnglishToGermanCard.OfNoun(noun, spacing, scheduler)
                | Vocab vocab ->
                    yield GermanToEnglishCard([Text vocab.Deutsch], [Text vocab.EnglishKey], vocab, spacing, scheduler)
                    yield EnglishToGermanCard([Text vocab.EnglishKey], [Text vocab.Deutsch], vocab, spacing, scheduler)

                | _ -> ()
        }
        |> Seq.filter (fun card -> filters |> List.forall (fun filters -> filters |> Seq.exists (fun f -> f.Filter card)))
        |> Seq.cast
        |> Seq.cache
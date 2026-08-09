namespace Loana.Desktop

open Loana.Language
open Loana.Data
open Loana.Vocab
open Loana.Verbs
open Loana.Desktop.Quizzes

type MenuSelection =
    | VocabGroup of string list
    | VerbMode
    | Quiz of Quiz

[<ReferenceEquality>]
type MenuFilter =
    {
        Name: string
        Apply: VocabDeck -> Card seq -> Card seq
    }

    static member val Options =
        [|
            { Name = "None"; Apply = fun _ cards -> cards }
            { Name = "New words only"; Apply = (fun vocab cards -> vocab.FilterByTier(cards, 1, 1)) }
            { Name = "Unlocks only"; Apply = (fun vocab cards -> vocab.FilterByTier(cards, 2, 999)) }
        |]

type MenuState =
    {
        mutable Running: bool
        Data: LoanaState
        Vocab: VocabDeck
        Quizzes: QuizScheduler
        Verbs: VerbCache
        SelectionOptions: MenuSelection array
        mutable Selection: MenuSelection
        mutable Filter: MenuFilter
        mutable BatchSize: int
    }

    member this.Words = this.Data.Words
    member this.Scheduler = this.Data.Scheduler

    static member Create(loana_state: LoanaState) : MenuState =
        let quizzes = QuizScheduler(loana_state.Scheduler)

        {
            Running = true
            Data = loana_state
            Vocab = VocabDeck(loana_state.Scheduler, loana_state.Words)
            Quizzes = quizzes
            Verbs = VerbCache(loana_state.Scheduler, loana_state.Words)
            SelectionOptions =
                seq {
                    for group in loana_state.Words.Groups do
                        yield VocabGroup(List.ofSeq group.WordlistNames)
                        yield! group.WordlistNames |> Seq.map List.singleton |> Seq.map VocabGroup

                    yield VocabGroup []
                    yield VerbMode

                    for quiz in quizzes.Quizzes do
                        yield Quiz quiz
                }
                |> Array.ofSeq
            Selection = VocabGroup []
            Filter = MenuFilter.Options.[0]
            BatchSize = 10
        }

    member this.FilteredWords(word_lists: string list) : Card seq =
        this.Filter.Apply this.Vocab (this.Vocab.AvailableCards(word_lists))

    member this.LearnBatchSize = this.BatchSize * 4
    member this.ReviewBatchSize = this.BatchSize * 10

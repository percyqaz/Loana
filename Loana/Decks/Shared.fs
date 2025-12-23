namespace Loana.Decks

open System
open Avalonia.Media
open Loana
open Loana.Scheduler
open Loana.Interface

type DeckFilter<'C> =
    { Label: string; Color: IBrush; Filter: 'C -> bool }
    static member OfCase(case: Case, filter: 'C -> Case) =
        { Label = case.ToString(); Color = case.Color; Filter = fun card -> filter card = case }
    static member OfGender(gender: Gender, filter: 'C -> Gender) =
        { Label = gender.ToString(); Color = gender.Color; Filter = fun card -> filter card = gender }
    static member OfPerson(person: Person, filter: 'C -> Person) =
        { Label = person.ToString(); Color = Brushes.White; Filter = fun card -> filter card = person }

type DeckFilterGroup<'C> =
    { Label: string; Filters: DeckFilter<'C> list }
    member this.Pick(from: Collections.Generic.HashSet<DeckFilter<'C>>) =
        this.Filters |> List.filter (from.Contains)

[<AbstractClass>]
type Deck() =
    abstract member Name : string
    abstract member Menu : CardScheduler * IOutput * IOutput -> Menu

[<AbstractClass>]
type Deck<'C when 'C :> Card>() =
    inherit Deck()
    abstract member Filters : DeckFilterGroup<'C> list
    abstract member Build : DeckFilter<'C> list list * CardScheduler -> Card seq

    override this.Menu(scheduler: CardScheduler, log: IOutput, output: IOutput) : Menu =
        DeckBuilderMenu(this, scheduler, log, output)

and DeckBuilderMenu<'C when 'C :> Card>(deck: Deck<'C>, scheduler: CardScheduler, log: IOutput, output: IOutput) =
    inherit Menu(output)

    let SESSION_SIZE = 50
    let session = Submenu.Create()
    let filters = deck.Filters
    let enabled_filters = filters |> Seq.map _.Filters |> Seq.concat |> Collections.Generic.HashSet<_>

    member private this.Draw() =
        output.Clear()
        output.WriteLine($" {deck.Name} ", Brushes.Black, Brushes.White)
        output.WriteLine("Filters:")
        for group in filters do
            output.Write($" {group.Label}: ")
            for filter in group.Filters do
                if enabled_filters.Contains(filter) then
                    output.Button($" {filter.Label} ", filter.Label, filter.Color, Brush.Parse("#303030"))
                    output.Button(" ", filter.Label, filter.Color, filter.Color)
                else
                    output.Button($" {filter.Label}  ", filter.Label, filter.Color, Brush.Parse("#101010"))
                output.Write(" ")
            output.WriteLine()
        output.WriteLine($"Session size: {SESSION_SIZE}")

        let available = deck.Build(filters |> List.map (fun f -> f.Pick enabled_filters), scheduler)
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        let due = available |> Seq.where (fun card -> card.IsDue now) |> Seq.length

        output.Write($" {available |> Seq.length} available ", Brushes.Black, Brushes.White)
        output.Write(" ")
        output.Write($" {due} due ", Brushes.LimeGreen, Brushes.DarkGreen)
        output.WriteLine()
        output.WriteLine()
        output.Button(" ok ", "ok", Brushes.LightGray, Brush.Parse("#101010"))
        output.Write(" ")
        output.Button(" back ", "back", Brushes.LightGray, Brush.Parse("#101010"))

    override this.Start() : bool = this.Draw(); true

    override this.Next(user_input: string) : bool =
        if session.HasMenu then
            session.Next(user_input)
        else
            match user_input with
            | "back" -> false
            | "ok" ->
                session.Open(ReviewSession(CardStack.Build(deck.Build(filters |> List.map (fun f -> f.Pick enabled_filters), scheduler), true, SESSION_SIZE), log, output))
                session.HasMenu
            | _ ->

                match filters |> Seq.map (fun group -> group.Filters) |> Seq.concat |> Seq.tryFind(fun f -> f.Label.Equals(user_input, StringComparison.Ordinal)) with
                | Some f ->
                    if enabled_filters.Contains(f) then enabled_filters.Remove(f) else enabled_filters.Add(f)
                    |> ignore
                | None -> ()
                this.Draw(); true

[<AutoOpen>]
module internal ArticleConstants =

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

    let KLEIN : Adjective = { Deutsch = "klein"; English = "small" }

type EnglishToGermanCard(front: AnnotationTree, back: AnnotationTree, key: string, spacing_rule: CardSpacingRule, scheduler: CardScheduler) =
    inherit Card(key, spacing_rule, scheduler)

    override this.DisplayFront(output: IOutput) : unit =

        AnnotationTree.render(front, output)

        output.Write(" -> English ", AnnotationTree.gradient Colors.Red Colors.Black, Brushes.White)
        output.Write(" ")
        if this.Schedule.LearningStep.IsSome then
            output.WriteLine(" Learning ", Brushes.Black, Brushes.Cyan)

    override this.DisplayBack(output: IOutput): unit =
        AnnotationTree.render(back, output)

    override this.FrontInput(user_input: string, output: IOutput) : CardEase option =
        if user_input = AnnotationTree.flatten_tree back then
            Some CardEase.Okay
        else
            output.WriteLine(" Mistake! See below: ", Brushes.Black, Brushes.Red)
            output.WriteLine(user_input, Brushes.LightPink)
            None

    override this.BackInput(user_input: string, output: IOutput) : CardEase = CardEase.Forgot
namespace Loana.Decks

open System
open Avalonia.Media
open Loana
open Loana.Scheduler
open Loana.Interface

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

type DeckFilter<'C> =
    { Label: string; Color: IBrush; Filter: 'C -> bool }
    static member OfCase(case: Case, filter: 'C -> Case) =
        { Label = case.ToString(); Color = case.Color; Filter = fun card -> filter card = case }
    static member OfGender(gender: Gender, filter: 'C -> Gender) =
        { Label = gender.ToString(); Color = gender.Color; Filter = fun card -> filter card = gender }

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
                output.Write($" {filter.Label} ", filter.Color, if enabled_filters.Contains(filter) then Brush.Parse("#303030") else Brush.Parse("#101010"))
                output.Write(" ")
            output.WriteLine("")
        output.WriteLine($"Session size: {SESSION_SIZE}")

        let available = deck.Build(filters |> List.map (fun f -> f.Pick enabled_filters), scheduler)
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        let due = available |> Seq.where (fun card -> card.IsDue now) |> Seq.length

        output.Write($" {available |> Seq.length} available ", Brushes.Black, Brushes.White)
        output.Write(" ")
        output.Write($" {due} due ", Brushes.LimeGreen, Brushes.DarkGreen)
        output.WriteLine(" ")

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

                match filters |> Seq.map (fun group -> group.Filters) |> Seq.concat |> Seq.tryFind(fun f -> f.Label.Equals(user_input, StringComparison.InvariantCultureIgnoreCase)) with
                | Some f ->
                    if enabled_filters.Contains(f) then enabled_filters.Remove(f) else enabled_filters.Add(f)
                    |> ignore
                | None -> ()
                this.Draw(); true

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

//let DECKS : Map<string, CardPermutation -> bool> = Map.ofList [
//    "[1*] basic 'the'", fun card ->
//        card.Type.IsDefinite && not card.Type.HasAdjective && card.Case.IsNominative
//    "[2*] 'the'", fun card ->
//        card.Type.IsDefinite && not card.Type.HasAdjective
//    "[4*] 'the' + adjective", fun card ->
//        card.Type.IsDefinite && card.Type.HasAdjective
//    "[1*] basic 'a'", fun card ->
//        card.Type.IsIndefinite && not card.Type.HasAdjective && card.Case.IsNominative
//    "[2*] 'a'", fun card ->
//        card.Type.IsIndefinite && not card.Type.HasAdjective
//    "[4*] 'a' + adjective", fun card ->
//        card.Type.IsIndefinite && card.Type.HasAdjective
//    "[3*] 'a' + 'the' combo", fun card ->
//        (card.Type.IsDefinite || card.Type.IsIndefinite) && not card.Type.HasAdjective
//    "[5*] 'a' + 'the' adjective combo", fun card ->
//        (card.Type.IsDefinite || card.Type.IsIndefinite)
//    "[1*] basic personal pronouns", fun card ->
//        card.Type.IsPerson && card.Case.IsNominative
//    "[2*] personal pronouns", fun card ->
//        card.Type.IsPerson
//    "[3*] basic possessive pronouns", fun card ->
//        card.Type.IsPossessive && not card.Type.HasAdjective && card.Case.IsNominative
//    "[4*] possessive pronouns", fun card ->
//        card.Type.IsPossessive && not card.Type.HasAdjective
//    "[6*] possessive pronouns + adjective", fun card ->
//        card.Type.IsPossessive && card.Type.HasAdjective
//    "[4*] tiny bit of everything", fun card ->
//        not card.Type.HasAdjective && card.Case.IsNominative
//    "[6*] tiny bit of everything + adjective", fun card ->
//        (card.Type.HasAdjective || card.Type.IsPerson) && card.Case.IsNominative
//    "[8*] everything", fun card ->
//        card.Type.HasAdjective || card.Type.IsPerson
//
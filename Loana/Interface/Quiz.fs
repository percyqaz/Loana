namespace Loana.Interface

open System
open Avalonia
open Avalonia.Media
open Loana.Core

module Quiz =

    let internal gradient (start: Color) (finish: Color) : IBrush =
        let brush = LinearGradientBrush()
        brush.StartPoint <- RelativePoint(0, 0, RelativeUnit.Relative)
        brush.StartPoint <- RelativePoint(1, 0, RelativeUnit.Relative)
        brush.GradientStops <- GradientStops()
        brush.GradientStops.Add(GradientStop(start, 0.0))
        brush.GradientStops.Add(GradientStop(finish, 1.0))
        brush

    type internal ConsoleAnnotationFragment = {
        Text: string
        Start: int
        Finish: int
        Color: IBrush
        Layer: int
    }

    let internal render_annotations (annotations: AnnotationTree, output: IOutput) : unit =
        let frags = ResizeArray<ConsoleAnnotationFragment>()

        let mutable position = 0
        let rec walk (fragments: AnnotationTree) : int =
            let mutable highest_layer = 0
            for fragment in fragments do
                let l =
                    match fragment with
                    | Text str ->
                        frags.Add { Text = str; Start = position; Finish = position + str.Length; Color = Brushes.White; Layer = 0 }
                        position <- position + str.Length
                        0
                    | Gender (gender, children) ->
                        let start = position
                        let layer = 1 + walk children
                        frags.Add {
                            Text = gender.ToString()
                            Start = start
                            Finish = position
                            Color =
                                match gender with
                                | Gender.Masculine -> gradient Colors.Blue Colors.Purple
                                | Gender.Neuter -> gradient Colors.Gray Colors.LightGray
                                | Gender.Feminine -> gradient Colors.Magenta Colors.Pink
                                | Gender.Plural -> gradient Colors.Yellow Colors.Wheat
                            Layer = layer
                        }
                        layer
                    | Case (case, children) ->
                        let start = position
                        let layer = 1 + walk children
                        frags.Add {
                            Text = case.Shorthand
                            Start = start
                            Finish = position
                            Color =
                                match case with
                                | Case.Nominative -> Brushes.Green
                                | Case.Accusative -> Brushes.Cyan
                                | Case.Dative -> Brushes.DarkMagenta
                                | Case.Genitive -> Brushes.Gold
                            Layer = layer
                        }
                        layer
                    | StrongDeclension children ->
                        let start = position
                        let layer = 1 + walk children
                        frags.Add {
                            Text = "S"
                            Start = start
                            Finish = position
                            Color = Brushes.Red
                            Layer = layer
                        }
                        layer
                    | WeakDeclension children ->
                        let start = position
                        let layer = 1 + walk children
                        frags.Add {
                            Text = "W"
                            Start = start
                            Finish = position
                            Color = Brushes.DarkCyan
                            Layer = layer
                        }
                        layer
                    | ArticleDeclension children ->
                        let start = position
                        let layer = 1 + walk children
                        frags.Add {
                            Text = "D"
                            Start = start
                            Finish = position
                            Color = Brushes.OrangeRed
                            Layer = layer
                        }
                        layer
                    | Annotation (note, children) ->
                        let start = position
                        let layer = 1 + walk children
                        frags.Add {
                            Text = note
                            Start = start
                            Finish = position
                            Color = Brushes.Gray
                            Layer = layer
                        }
                        layer
                highest_layer <- max highest_layer l
            highest_layer

        walk (annotations) |> ignore
        let lines = frags |> Seq.groupBy (fun x -> x.Layer) |> Seq.sortBy fst |> Seq.map (snd >> Seq.sortBy _.Start >> Array.ofSeq) |> Seq.toArray
        let render_line (line: ConsoleAnnotationFragment array) : unit =
            let mutable p = 0
            for frag in line do
                output.Write(String.replicate (frag.Start - p) " ", null)

                if frag.Layer = 0 then output.Write(frag.Text, null)
                else
                    let padded =
                        if frag.Text.Length <= (frag.Finish - frag.Start) then
                            let lpadding = ((frag.Finish - frag.Start) - frag.Text.Length) / 2
                            let rpadding = ((frag.Finish - frag.Start) - frag.Text.Length + 1) / 2
                            String.replicate lpadding "-" + frag.Text + String.replicate rpadding "-"
                        else
                            frag.Text.Substring(0, frag.Finish - frag.Start)
                    output.Write(padded, frag.Color)
                p <- frag.Finish
            output.WriteLine("", null)
        lines |> Array.iter render_line

type MenuContext<'T> =
    private {
        Output: IOutput
        Options: Map<string, 'T>
        Callback: Action<'T>
        mutable Selected: int
    }

    member this.Draw() : unit =
        let keys = Array.ofSeq this.Options.Keys
        this.Output.Clear()
        for i = 0 to keys.Length - 1 do
            if i = this.Selected then
                this.Output.Write(" > ", Brushes.White)
                this.Output.Write(keys.[i], Brushes.Yellow)
                this.Output.WriteLine(" <", Brushes.White)
            else
                this.Output.WriteLine(sprintf "%02i %s" ((keys.Length + i - this.Selected) % keys.Length) keys.[i], Brushes.LightGray)

    member this.Next(user_input: string) : unit =
        let keys = Array.ofSeq this.Options.Keys
        match Int32.TryParse(user_input) with
        | true, n ->
            this.Selected <- ((this.Selected + n) % keys.Length + keys.Length) % keys.Length
            this.Draw()
        | false, _ -> this.Callback.Invoke(this.Options.[keys.[this.Selected]])

type MenuContext =
    static member Create(options: Map<string, 'T>, callback: Action<'T>, output: IOutput) : MenuContext<'T> =
        {
            Output = output
            Options = options
            Callback = callback
            Selected = 0
        }

    static member CreateModePicker(callback: Action<(Loana.Cards.CardPool.CardPermutation -> bool)>, output: IOutput) : MenuContext<Loana.Cards.CardPool.CardPermutation -> bool> =
        MenuContext.Create(Loana.Cards.CardPool.MODES, callback, output)

[<RequireQualifiedAccess>]
type QuizState =
    | Start
    | ShowingFront
    | ShowingBack
    | Complete

type QuizContext =
    private {
        Log: IOutput
        Display: IOutput
        Pool: ResizeArray<Card>
        mutable Current: Card
        mutable State: QuizState
    }

    static member Create(pool: ResizeArray<Card>, log: IOutput, display: IOutput) : QuizContext =
        let current = pool.[0]
        pool.RemoveAt(0)
        {
            Log = log
            Display = display
            Pool = pool
            Current = current
            State = QuizState.Start
        }

    static member CreateFromMode(mode: Loana.Cards.CardPool.CardPermutation -> bool, log: IOutput, display: IOutput): QuizContext =
        let pool =
            Loana.Cards.CardPool.generate_card_pool ()
            |> Seq.filter mode
            |> Seq.randomShuffle
            |> Seq.map _.Generate
            |> ResizeArray
        QuizContext.Create(pool, log, display)

    member private this.DisplayCard (card: Card) : unit =
        this.Display.Clear()
        Quiz.render_annotations(card.Front, this.Display)

    member private this.NextCard() : bool =
        if this.Pool.Count > 0 then
            this.Current <- this.Pool.[0]
            this.State <- QuizState.ShowingFront
            this.Pool.RemoveAt(0)
            this.DisplayCard(this.Current)
            true
        else
            this.State <- QuizState.Complete
            this.Log.WriteLine("All done!", Brushes.Green)
            false

    member this.Next(user_input: string) : bool =
        match this.State with
        | QuizState.Start ->
            this.Log.WriteLine(sprintf "Beginning quiz consisting of %i cards" (this.Pool.Count + 1), Brushes.LightGreen)
            this.DisplayCard(this.Current)
            this.State <- QuizState.ShowingFront
            true
        | QuizState.ShowingFront ->
            let correct_answer = AnnotationTree.flatten_tree this.Current.Back
            // todo: rules on case-sensitivity
            if user_input = correct_answer then
                // Allow this card to be discarded
                this.NextCard()
            else
                this.Display.WriteLine(user_input, Brushes.LightPink)
                Quiz.render_annotations(this.Current.Back, this.Display)
                this.State <- QuizState.ShowingBack
                true
        | QuizState.ShowingBack ->
            // todo: based on user input certain commands could bury, skip, or allow the mistake
            // Reinsert the card into the pool
            this.Log.WriteLine("Requeueing failed card", Brushes.LightPink)
            this.Pool.Insert(min this.Pool.Count 5, this.Current)
            this.NextCard()
        | QuizState.Complete -> false
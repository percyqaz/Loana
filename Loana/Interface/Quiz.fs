namespace Loana.Interface

open System
open Avalonia.Media
open Loana.Core

type ConsoleAnnotationFragment = {
    Text: string
    Start: int
    Finish: int
    Color: IBrush
    Layer: int
}

module Quiz =

    let render_annotations (annotations: AnnotationTree, output: IOutput) : unit =
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
                                | Gender.Masculine -> Brushes.Blue
                                | Gender.Neuter -> Brushes.Gray
                                | Gender.Feminine -> Brushes.Magenta
                                | Gender.Plural -> Brushes.Yellow
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

    let display_card (card: Card, output: IOutput) : bool =
        output.Clear()
        render_annotations(card.Front, output)
        let user_answer = Console.ReadLine()
        let correct_answer = AnnotationTree.flatten_tree card.Back
        if user_answer = correct_answer then true
        else
            render_annotations(card.Back, output)
            Console.ReadLine() |> ignore
            false

    type ConsoleOutput() =
        interface IOutput with
            member this.Write(text, brush) = Console.Write(text)
            member this.WriteLine(text, brush) = Console.WriteLine(text)
            member this.Clear() = Console.Clear()

    let run_quiz (cards: ResizeArray<Card>) =
        let output = ConsoleOutput() :> IOutput
        output.WriteLine(sprintf "Beginning quiz consisting of %i cards" cards.Count, null)
        Console.ReadLine() |> ignore

        while cards.Count > 0 do
            let next_card = cards.[0]
            cards.RemoveAt(0)
            let success = display_card(next_card, output)
            if not success then
                cards.Insert(min cards.Count 5, next_card)
        output.WriteLine("All done!", null)

    let mutable selected = 0
    let pick_mode(modes: Map<string, _>) : string * _ =
        let keys = Array.ofSeq modes.Keys
        selected <- selected % keys.Length
        let mutable chosen = false

        while not chosen do
            Console.Clear()
            for i = 0 to keys.Length - 1 do
                if i = selected then
                    printfn " > %s <" (keys.[i])
                else
                    printfn "%02i %s" ((keys.Length + i - selected) % keys.Length) keys.[i]
            let choice = Console.ReadLine()
            match Int32.TryParse(choice) with
            | true, n -> selected <- ((selected + n) % keys.Length + keys.Length) % keys.Length
            | false, _ -> chosen <- true

        keys.[selected], modes.[keys.[selected]]

    let example() =
        Loana.Cards.CardPool.generate_card_pool ()
        |> Seq.filter Loana.Cards.CardPool.MODES.[Loana.Cards.CardPool.MODES.Keys |> Seq.head]
        |> Seq.randomShuffle
        |> Seq.map _.Generate
        |> ResizeArray
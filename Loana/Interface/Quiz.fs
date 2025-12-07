namespace Loana.Interface

open System
open System.Drawing
open System.Runtime.CompilerServices
open Loana.Core

[<Extension>]
type StringExtensions =

    [<Extension>]
    static member WithColor(text: string, color: Color) : string =
        sprintf "\u001b[38;2;%d;%d;%dm%s\u001b[0m" color.R color.G color.B text

module Quiz =

    type ConsoleAnnotationFragment = {
        Text: string
        Start: int
        Finish: int
        Color: Color
        Layer: int
    }

    let walk_annotations (annotations: AnnotationTree) : string array =
        let output = ResizeArray<ConsoleAnnotationFragment>()

        let mutable position = 0
        let rec walk (fragments: AnnotationTree) : int =
            let mutable highest_layer = 0
            for fragment in fragments do
                let l =
                    match fragment with
                    | Text str ->
                        output.Add { Text = str; Start = position; Finish = position + str.Length; Color = Color.White; Layer = 0 }
                        position <- position + str.Length
                        0
                    | Gender (gender, children) ->
                        let start = position
                        let layer = 1 + walk children
                        output.Add {
                            Text = gender.ToString()
                            Start = start
                            Finish = position
                            Color =
                                match gender with
                                | Gender.Masculine -> Color.Blue
                                | Gender.Neuter -> Color.Gray
                                | Gender.Feminine -> Color.Magenta
                                | Gender.Plural -> Color.Yellow
                            Layer = layer
                        }
                        layer
                    | Case (case, children) ->
                        let start = position
                        let layer = 1 + walk children
                        output.Add {
                            Text = case.Shorthand
                            Start = start
                            Finish = position
                            Color =
                                match case with
                                | Case.Nominative -> Color.Green
                                | Case.Accusative -> Color.Cyan
                                | Case.Dative -> Color.DarkMagenta
                                | Case.Genitive -> Color.Gold
                            Layer = layer
                        }
                        layer
                    | StrongDeclension children ->
                        let start = position
                        let layer = 1 + walk children
                        output.Add {
                            Text = "S"
                            Start = start
                            Finish = position
                            Color = Color.Red
                            Layer = layer
                        }
                        layer
                    | WeakDeclension children ->
                        let start = position
                        let layer = 1 + walk children
                        output.Add {
                            Text = "W"
                            Start = start
                            Finish = position
                            Color = Color.DarkCyan
                            Layer = layer
                        }
                        layer
                    | ArticleDeclension children ->
                        let start = position
                        let layer = 1 + walk children
                        output.Add {
                            Text = "D"
                            Start = start
                            Finish = position
                            Color = Color.OrangeRed
                            Layer = layer
                        }
                        layer
                    | Annotation (note, children) ->
                        let start = position
                        let layer = 1 + walk children
                        output.Add {
                            Text = note
                            Start = start
                            Finish = position
                            Color = Color.Gray
                            Layer = layer
                        }
                        layer
                highest_layer <- max highest_layer l
            highest_layer

        walk (annotations) |> ignore
        let lines = output |> Seq.groupBy (fun x -> x.Layer) |> Seq.sortBy fst |> Seq.map (snd >> Seq.sortBy _.Start >> Array.ofSeq) |> Seq.toArray
        let format_line (line: ConsoleAnnotationFragment array) : string =
            let mutable s = ""
            let mutable p = 0
            for frag in line do
                s <- s + String.replicate (frag.Start - p) " "

                if frag.Layer = 0 then s <- s + frag.Text
                else
                    let padded =
                        if frag.Text.Length <= (frag.Finish - frag.Start) then
                            let lpadding = ((frag.Finish - frag.Start) - frag.Text.Length) / 2
                            let rpadding = ((frag.Finish - frag.Start) - frag.Text.Length + 1) / 2
                            String.replicate lpadding "-" + frag.Text + String.replicate rpadding "-"
                        else
                            frag.Text.Substring(0, frag.Finish - frag.Start)
                    s <- s + padded.WithColor(frag.Color)
                p <- frag.Finish
            s
        lines |> Array.map format_line

    let render_annotations (annotations: AnnotationTree) : unit =
        for line in walk_annotations annotations do
            printfn "%s" line

    let display_card (card: Card) : bool =
        Console.Clear()
        render_annotations card.Front
        let user_answer = Console.ReadLine()
        let correct_answer = AnnotationTree.flatten_tree card.Back
        if user_answer = correct_answer then true
        else
            render_annotations card.Back
            Console.ReadLine() |> ignore
            false

    let run_quiz (cards: ResizeArray<Card>) =
        printfn "Beginning quiz consisting of %i cards" cards.Count
        Console.ReadLine() |> ignore

        while cards.Count > 0 do
            let next_card = cards.[0]
            cards.RemoveAt(0)
            let success = display_card next_card
            if not success then
                cards.Insert(min cards.Count 5, next_card)
        printfn "%s" ("All done!".WithColor(Color.Green))

    let mutable selected = 0
    let pick_mode(modes: Map<string, _>) : string * _ =
        let keys = Array.ofSeq modes.Keys
        selected <- selected % keys.Length
        let mutable chosen = false

        while not chosen do
            Console.Clear()
            for i = 0 to keys.Length - 1 do
                if i = selected then
                    printfn " > %s <" (keys.[i].WithColor(Color.Yellow))
                else
                    printfn "%02i %s" ((keys.Length + i - selected) % keys.Length) keys.[i]
            let choice = Console.ReadLine()
            match Int32.TryParse(choice) with
            | true, n -> selected <- ((selected + n) % keys.Length + keys.Length) % keys.Length
            | false, _ -> chosen <- true

        keys.[selected], modes.[keys.[selected]]
namespace Loana

open Avalonia
open Avalonia.Media

type AnnotationFragment =
    | Text of string
    | Gender of Gender * AnnotationTree
    | Case of Case * AnnotationTree
    | StrongDeclension of AnnotationTree
    | WeakDeclension of AnnotationTree
    | ArticleDeclension of AnnotationTree
    | Annotation of string * AnnotationTree

and AnnotationTree = AnnotationFragment list

module AnnotationTree =

    let rec flatten_fragment (fragment: AnnotationFragment) : string =
        match fragment with
        | Text str -> str
        | Gender (_, children) -> flatten_tree children
        | Case (_, children) -> flatten_tree children
        | StrongDeclension children -> flatten_tree children
        | WeakDeclension children -> flatten_tree children
        | ArticleDeclension children -> flatten_tree children
        | Annotation (_, children) -> flatten_tree children

    and flatten_tree (tree: AnnotationTree) : string =
        List.map flatten_fragment tree
        |> String.concat ""

    //
    let internal gradient (start: Color) (finish: Color) : IBrush =
        let brush = LinearGradientBrush()
        brush.StartPoint <- RelativePoint(0, 0, RelativeUnit.Relative)
        brush.StartPoint <- RelativePoint(1, 0, RelativeUnit.Relative)
        brush.GradientStops <- GradientStops()
        brush.GradientStops.Add(GradientStop(start, 0.0))
        brush.GradientStops.Add(GradientStop(finish, 1.0))
        brush

    type internal ConsoleAnnotationFragment =
        {
            Text: string
            Start: int
            Finish: int
            Color: IBrush
            Layer: int
        }

    let internal render (annotations: AnnotationTree, output: IOutput) : unit =
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
                            Color = gender.Color
                            Layer = layer
                        }
                        layer
                    | Case (case, children) ->
                        let start = position
                        let layer = 1 + walk children
                        frags.Add {
                            Text = case.ToString()
                            Start = start
                            Finish = position
                            Color = case.Color
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
                output.Write(String.replicate (frag.Start - p) " ")

                if frag.Layer = 0 then output.Write(frag.Text)
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
            output.WriteLine("")
        lines |> Array.iter render_line
namespace Loana.Desktop.Quizzes

open System.Drawing
open Loana.Language
open Loana.Desktop.CLI

type internal ConsoleAnnotationFragment =
    {
        Text: string
        Start: int
        Finish: int
        Color: Color
        Layer: int
    }

module AnnotationTreeRenderer =

    let to_question_side (annotations: AnnotationTree, background: Color) : QuestionSide =
        let frags = ResizeArray<ConsoleAnnotationFragment>()

        let mutable position = 0

        let rec walk (fragments: AnnotationTree) : int =
            let mutable highest_layer = 0

            for fragment in fragments do
                let l =
                    match fragment with
                    | Text str ->
                        frags.Add(
                            {
                                Text = str
                                Start = position
                                Finish = position + str.Length
                                Color = Color.White
                                Layer = 0
                            }
                        )

                        position <- position + str.Length
                        0
                    | Gender(gender, children) ->
                        let start = position
                        let layer = 1 + walk children

                        frags.Add(
                            {
                                Text = gender.ToString()
                                Start = start
                                Finish = position
                                Color = gender.Color
                                Layer = layer
                            }
                        )

                        layer
                    | Case(case, children) ->
                        let start = position
                        let layer = 1 + walk children

                        frags.Add(
                            {
                                Text = case.ToString()
                                Start = start
                                Finish = position
                                Color = case.Color
                                Layer = layer
                            }
                        )

                        layer
                    | StrongDeclension children ->
                        let start = position
                        let layer = 1 + walk children

                        frags.Add(
                            {
                                Text = "S"
                                Start = start
                                Finish = position
                                Color = Color.Red
                                Layer = layer
                            }
                        )

                        layer
                    | WeakDeclension children ->
                        let start = position
                        let layer = 1 + walk children

                        frags.Add(
                            {
                                Text = "W"
                                Start = start
                                Finish = position
                                Color = Color.DarkCyan
                                Layer = layer
                            }
                        )

                        layer
                    | ArticleDeclension children ->
                        let start = position
                        let layer = 1 + walk children

                        frags.Add(
                            {
                                Text = "D"
                                Start = start
                                Finish = position
                                Color = Color.OrangeRed
                                Layer = layer
                            }
                        )

                        layer
                    | Annotation(note, children) ->
                        let start = position
                        let layer = 1 + walk children

                        frags.Add(
                            {
                                Text = note
                                Start = start
                                Finish = position
                                Color = Color.Gray
                                Layer = layer
                            }
                        )

                        layer

                highest_layer <- max highest_layer l

            highest_layer

        walk(annotations) |> ignore

        let lines =
            frags
            |> Seq.groupBy _.Layer
            |> Seq.sortBy fst
            |> Seq.map(snd >> Seq.sortBy _.Start >> Array.ofSeq)
            |> Seq.toList

        let render_line (line: ConsoleAnnotationFragment array) : QuestionLine =
            seq {
                let mutable p = 0

                for frag in line do
                    yield { Text = String.replicate (frag.Start - p) " "; FG = Color.White }

                    if frag.Layer = 0 then
                        yield { Text = frag.Text; FG = Color.White }
                    else
                        let padded =
                            if frag.Text.Length <= (frag.Finish - frag.Start) then
                                let lpadding = ((frag.Finish - frag.Start) - frag.Text.Length) / 2
                                let rpadding = ((frag.Finish - frag.Start) - frag.Text.Length + 1) / 2
                                String.replicate lpadding "-" + frag.Text + String.replicate rpadding "-"
                            else
                                frag.Text.Substring(0, frag.Finish - frag.Start)

                        yield { Text = padded; FG = frag.Color }

                    p <- frag.Finish
            }
            |> List.ofSeq
            |> QuestionLine.Create

        {
            Lines = QuestionLine.Create([]) :: (lines |> List.map render_line) @ [ QuestionLine.Create([]) ]
            BG = background
        }

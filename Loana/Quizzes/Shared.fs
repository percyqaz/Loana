namespace Loana.Quizzes

open System.Drawing
open Loana.CLI
open Loana.Language
open Loana.Data

[<AutoOpen>]
module internal ArticleConstants =

    let SPOON = Wordlist.parse_noun "Löffel = spoon :m plural Löffel = spoons"
    let FORK = Wordlist.parse_noun "Gabel = fork :f plural Gabeln = forks"
    let KNIFE = Wordlist.parse_noun "Messer = knife :n plural Messer = knives"
    let NOUNS : Noun array =
        [|
            SPOON
            FORK
            KNIFE
            SPOON.PluralForm.Value
            FORK.PluralForm.Value
            KNIFE.PluralForm.Value
        |]

    let KLEIN : Adjective = { Translation = Wordlist.parse_vocab "klein = small" }

type GermanPracticeCard =

    static let GERMAN_BG = Color.FromArgb(0x400000)
    static let ANSWER_BG = Color.FromArgb(0x202020)

    static member Create(front: AnnotationTree, back: AnnotationTree) : QuizCard =
        {
            Front =
                let f = (AnnotationTree.flatten_tree front).Length in
                AnnotationTree.render(front, GERMAN_BG)
                |> Seq.map (fun l -> { BG = GERMAN_BG; Content = l; Length = f })
                |> List.ofSeq
            Back =
                let b = (AnnotationTree.flatten_tree back).Length in
                AnnotationTree.render(back, ANSWER_BG)
                |> Seq.map (fun l -> { BG = ANSWER_BG; Content = l; Length = b })
                |> List.ofSeq
            Answer = AnnotationTree.flatten_tree back
        }
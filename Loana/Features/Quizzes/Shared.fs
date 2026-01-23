namespace Loana.Features

open System.Drawing
open Loana.CLI
open Loana.Language

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

    static member Create(front: AnnotationTree, back: AnnotationTree) : CliCard =
        {
            Front =
                fun () ->
                    AnnotationTree.render(front)
                    Console.WriteLine(" -> German ", Color.LightGoldenrodYellow, Color.DarkRed)
            Back =
                fun () ->
                    AnnotationTree.render(back)
            Answer = AnnotationTree.flatten_tree back
        }
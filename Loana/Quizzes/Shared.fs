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

    static let GERMAN_BG = Color.FromArgb(0x200000)
    static let ANSWER_BG = Color.FromArgb(0x050505)

    static member Create(front: AnnotationTree, back: AnnotationTree) : Question =
        {
            Front = AnnotationTree.to_question_side(front, GERMAN_BG)
            Back = AnnotationTree.to_question_side(back, ANSWER_BG)
            Answer = AnnotationTree.flatten_tree back
        }
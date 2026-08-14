namespace Loana.Desktop.Quizzes

open System.Drawing
open Loana.Language

[<AutoOpen>]
module internal ArticleConstants =

    let SPOON = Noun.FromString("Löffel = spoon :m plural Löffel = spoons")
    let FORK = Noun.FromString("Gabel = fork :f plural Gabeln = forks")
    let KNIFE = Noun.FromString("Messer = knife :n plural Messer = knives")

    let NOUNS: Noun array =
        [|
            SPOON
            FORK
            KNIFE
            SPOON.PluralForm.Value
            FORK.PluralForm.Value
            KNIFE.PluralForm.Value
        |]

    let KLEIN: Adjective = { Translation = Vocab.FromString("klein = small") }

type GermanPracticeQuestion =

    static let GERMAN_BG = Color.FromArgb(0xFF_200000)
    static let ANSWER_BG = Color.FromArgb(0xFF_050505)

    static member Create(front: AnnotationTree, back: AnnotationTree) : Question =
        {
            Front = AnnotationTreeRenderer.to_question_side(front, GERMAN_BG)
            Back = AnnotationTreeRenderer.to_question_side(back, ANSWER_BG)
            Answer = AnnotationTree.flatten_tree back
        }

[<CustomEquality; NoComparison>]
type Quiz =
    {
        Name: string
        Key: string
        Questions: unit -> Question array
    }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Quiz as q -> q.Name = this.Name
        | _ -> false

    override this.GetHashCode() : int = this.Name.GetHashCode()

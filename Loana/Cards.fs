namespace Loana.Core

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

type Card = {
    Front: AnnotationTree
    Back: AnnotationTree
}
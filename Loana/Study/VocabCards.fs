namespace Loana.Study

open Loana.Language
open Loana.GUI

type VocabCard =

    static member M_Tier1_RecogniseDE(v: Vocab) = { Key = $"vocab-recognise-{v.Key}"; Tier = 1 }
    static member C_Tier1_RecogniseDE(v: Vocab) =
        let annotation_html(a: Annotation) =
            match a.Note with
            | Some n -> $"""{a.Text} <span class="note">[{n}]</span>"""
            | None -> a.Text
        let en_html = (v.English :: v.EnglishAlternatives) |> Seq.map annotation_html |> String.concat ", "
        {
            Meta = VocabCard.M_Tier1_RecogniseDE(v: Vocab)
            Front =
                $"""
                <div class="de-en">
                <div class="de">{v.Deutsch}</div>
                <div class="en">???</div>
                </div>
                """
            Back =
                $"""
                <div class="de-en">
                <div class="de">{v.Deutsch}</div>
                <div class="en">{en_html}</div>
                </div>
                """
        }

    static member M_Tier2_RecallDE(v: Vocab) = { Key = $"vocab-recall-{v.Key}"; Tier = 2 }
    static member C_Tier2_RecallDE(v: Vocab) =
        let annotation_html(a: Annotation) =
            match a.Note with
            | Some n -> $"""{a.Text} <span class="note">[{n}]</span>"""
            | None -> a.Text
        let en_html = (v.English :: v.EnglishAlternatives) |> Seq.map annotation_html |> String.concat ", "
        {
            Meta = VocabCard.M_Tier2_RecallDE(v)
            Front =
                $"""
                <div class="en-de">
                <div class="en">{en_html}</div>
                <div class="de">???</div>
                </div>
                """
            Back =
                $"""
                <div class="en-de">
                <div class="en">{en_html}</div>
                <div class="de">{v.Deutsch}</div>
                </div>
                """
        }

    static member M_Tier3_RecogniseArticleDE(n: Noun) = { Key = $"noun-recognise-{n.KeyWithGender}"; Tier = 3 }
    static member C_Tier3_RecogniseArticleDE(n: Noun) =
        let annotation_html(a: Annotation) =
            match a.Note with
            | Some n -> $"""<span class="note">the </span>{a.Text} <span class="note">[{n}]</span>"""
            | None -> $"""<span class="note">the </span>{a.Text}"""
        let de_html =
            let article = AnnotationTree.flatten_tree (Deutsch.definite_article n.Guts.Gender Case.Nominative)
            $"""<span class="note">{article} </span><span style="color:#{n.Guts.Gender.Color.ToArgb().ToString("X06")};">{n.Deutsch}</span>"""
        let en_html = (n.English :: n.EnglishAlternatives) |> Seq.map annotation_html |> String.concat ", "
        {
            Meta = VocabCard.M_Tier3_RecogniseArticleDE(n)
            Front =
                $"""
                <div class="de-en">
                <div class="de">{de_html}</div>
                <div class="en">???</div>
                </div>
                """
            Back =
                $"""
                <div class="de-en">
                <div class="de">{de_html}</div>
                <div class="en">{en_html}</div>
                </div>
                """
        }

    static member M_Tier4_RecallArticleDE(n: Noun) = { Key = $"noun-recall-{n.KeyWithGender}"; Tier = 4 }
    static member C_Tier4_RecallArticleDE(n: Noun) =
        let annotation_html(a: Annotation) =
            match a.Note with
            | Some n -> $"""<span class="note">the </span>{a.Text} <span class="note">[{n}]</span>"""
            | None -> $"""<span class="note">the </span>{a.Text}"""
        let de_html =
            let article = AnnotationTree.flatten_tree (Deutsch.definite_article n.Guts.Gender Case.Nominative)
            $"""<span class="note">{article} </span><span style="color:#{n.Guts.Gender.Color.ToArgb().ToString("X06")};">{n.Deutsch}</span>"""
        let en_html = (n.English :: n.EnglishAlternatives) |> Seq.map annotation_html |> String.concat ", "
        {
            Meta = VocabCard.M_Tier4_RecallArticleDE(n)
            Front =
                $"""
                <div class="en-de">
                <div class="en">{en_html}</div>
                <div class="de">???</div>
                </div>
                """
            Back =
                $"""
                <div class="en-de">
                <div class="en">{en_html}</div>
                <div class="de">{de_html}</div>
                </div>
                """
        }
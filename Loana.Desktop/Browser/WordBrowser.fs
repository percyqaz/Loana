namespace Loana.Desktop.Browser

open System
open System.Collections.Generic
open System.Drawing
open Loana.Data
open Loana.Desktop.CLI

type WordBrowserState =
    | SearchBox
    | Item of item: int

type WordBrowser(words: WordBank) =

    let mutable query = ""
    let mutable results: IReadOnlyList<WordlistEntry> = words.Entries
    let mutable position = 0

    [<Literal>]
    let PAGE_SIZE = 20

    let update_search_results () =
        let current_item =
            if position < results.Count then Some results.[position] else None

        results <-
            words.Entries
            |> Seq.where(fun x ->
                let t =
                    match x.Item with
                    | WordlistItem.Noun n -> n.Translation
                    | WordlistItem.Verb v -> v.Infinitive
                    | WordlistItem.Vocab v -> v

                t.Deutsch.Contains(query, StringComparison.InvariantCultureIgnoreCase)
                || t.EnglishAsciiIdentifier.Contains(query, StringComparison.InvariantCultureIgnoreCase)
            )
            |> ResizeArray

        position <-
            match current_item with
            | Some i -> Seq.tryFindIndex ((=) i) results |> Option.defaultValue 0
            | None -> 0

    member this.Run() : unit =
        let mutable loop = true

        while loop do
            MenuRender.UpdateWidth()
            MenuRender.Write(" Search: ".PadRight(MenuRender.Width - 14), Color.LightGray, Color.FromArgb(0xFF_101010))
            MenuRender.Write((sprintf "% 5i results " results.Count), Color.LightGray, Color.FromArgb(0xFF_101010))
            MenuRender.WriteLine()
            MenuRender.Write(" ", Color.White, Color.FromArgb(0xFF_101010))
            MenuRender.Write(query.PadRight(MenuRender.Width - 2), Color.White, Color.Black)
            MenuRender.Write(" ", Color.White, Color.FromArgb(0xFF_101010))
            MenuRender.WriteLine()
            MenuRender.WriteLine("".PadRight(MenuRender.Width), Color.White, Color.FromArgb(0xFF_101010))

            if results.Count = 0 then
                MenuRender.WriteLine(
                    " (No results) ".PadRight(MenuRender.Width),
                    Color.LightGray,
                    Color.FromArgb(0xFF_202020)
                )

            let start_index =
                min (results.Count - PAGE_SIZE) (position - PAGE_SIZE / 2) |> max 0

            let end_index = min results.Count (start_index + PAGE_SIZE) - 1

            for i = start_index to end_index do
                let result = results.[i]

                let tag, tag_color =
                    match result.Item with
                    | Noun _ -> "noun", Color.FromArgb(0xFF_ffddff)
                    | Vocab v when v.LooksLikeANoun -> "noun?", Color.FromArgb(0xFF_ffddff)
                    | Verb _ -> "verb", Color.FromArgb(0xFF_ddffdd)
                    | Vocab v when v.LooksLikeAVerb -> "verb?", Color.FromArgb(0xFF_ddffdd)
                    | Vocab _ -> "word", Color.White

                let tags_width = tag.Length + 2 + 1 + result.Source.WordlistName.Length + 2

                let bg = Color.FromArgb(if position = i then 0xFF_404020 else 0xFF_202020)

                MenuRender.Write(
                    result.Item.HighlightString()
                    + String.replicate (MenuRender.Width - result.Item.ToString().Length - tags_width |> max 0) " ",
                    Color.White,
                    bg
                )

                MenuRender.Write($" {result.Source.WordlistName} ", Color.LightBlue, Color.DarkBlue)
                MenuRender.Write(" ", Color.White, bg)
                MenuRender.Write($" {tag} ", tag_color, Color.FromArgb(0xFF_303030))
                MenuRender.WriteLine()

            MenuRender.Redraw()

            let next_key = Console.ReadKey(true)

            match next_key.Key with
            | ConsoleKey.Escape -> loop <- false
            | ConsoleKey.UpArrow when results.Count > 0 ->
                position <- if position = 0 then results.Count - 1 else position - 1
            | ConsoleKey.DownArrow when results.Count > 0 ->
                position <- if position + 1 = results.Count then 0 else position + 1
            | ConsoleKey.Enter -> () //quizzes.Study(quiz)
            | ConsoleKey.Backspace ->
                if next_key.Modifiers &&& ConsoleModifiers.Control = ConsoleModifiers.Control && query <> "" then
                    query <- query.Split() |> fun x -> Array.truncate (x.Length - 1) x |> String.concat " "
                    update_search_results()
                elif query <> "" then
                    query <- query.Substring(0, query.Length - 1)
                    update_search_results()
            | _ when next_key.KeyChar <> '\u0000' ->
                query <- query + next_key.KeyChar.ToString()
                update_search_results()
            | _ -> ()

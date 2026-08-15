namespace Loana.Desktop.Browser

open System
open System.Drawing
open Loana.Data
open Loana.Desktop.CLI

type WordBrowserState =
    | SearchBox
    | Item of item: int

type WordBrowser(words: WordBank) =

    let tab = SearchTab.Create(words)

    [<Literal>]
    let PAGE_SIZE = 20

    member this.Run() : unit =
        let mutable loop = true

        while loop do
            MenuRender.UpdateWidth()
            MenuRender.Write(" Search: ".PadRight(MenuRender.Width - 14), Color.LightGray, Color.FromArgb(0xFF_101010))
            MenuRender.Write((sprintf "% 5i results " tab.Results.Count), Color.LightGray, Color.FromArgb(0xFF_101010))
            MenuRender.WriteLine()
            MenuRender.Write(" ", Color.White, Color.FromArgb(0xFF_101010))
            MenuRender.Write(tab.Query.PadRight(MenuRender.Width - 2), Color.White, Color.Black)
            MenuRender.Write(" ", Color.White, Color.FromArgb(0xFF_101010))
            MenuRender.WriteLine()
            MenuRender.WriteLine("".PadRight(MenuRender.Width), Color.White, Color.FromArgb(0xFF_101010))

            if tab.Results.Count = 0 then
                MenuRender.WriteLine(
                    " (No results) ".PadRight(MenuRender.Width),
                    Color.LightGray,
                    Color.FromArgb(0xFF_202020)
                )

            let start_index =
                min (tab.Results.Count - PAGE_SIZE) (tab.Position - PAGE_SIZE / 2) |> max 0

            let end_index = min tab.Results.Count (start_index + PAGE_SIZE) - 1

            for i = start_index to end_index do
                let result = tab.Results.[i]

                let tag, tag_color =
                    match result.Item with
                    | Noun _ -> "noun", Color.FromArgb(0xFF_ffddff)
                    | Vocab v when v.LooksLikeANoun -> "noun?", Color.FromArgb(0xFF_ffddff)
                    | Verb _ -> "verb", Color.FromArgb(0xFF_ddffdd)
                    | Vocab v when v.LooksLikeAVerb -> "verb?", Color.FromArgb(0xFF_ddffdd)
                    | Vocab _ -> "word", Color.White

                let tags_width = tag.Length + 2 + 1 + result.Source.WordlistName.Length + 2

                let bg = Color.FromArgb(if tab.Position = i then 0xFF_404020 else 0xFF_202020)

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
            | ConsoleKey.UpArrow -> tab.Up()
            | ConsoleKey.DownArrow -> tab.Down()
            | _ ->
                if tab.Buffer.TryAddKey(next_key) then
                    tab.UpdateSearchResults(words)

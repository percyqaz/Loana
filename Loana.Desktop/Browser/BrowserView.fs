namespace Loana.Desktop.Browser

open System
open System.Drawing
open Loana.Data
open Loana.Desktop.CLI

type BrowserView(state: BrowserState) =

    member this.DrawSearchTab(tab: SearchTab, panel: PanelRender) : unit =

        let inline pad_to_width (text: string, plain_text: string, target_width: int) : string =
            (if plain_text.Length > target_width then plain_text.Substring(0, target_width) else text)
            + String.replicate (target_width - plain_text.Length |> max 0) " "

        let DISPLAY_RESULTS_COUNT = Console.BufferHeight - 2

        let start_index =
            min (tab.Results.Count - DISPLAY_RESULTS_COUNT) (tab.Position - DISPLAY_RESULTS_COUNT / 2) |> max 0

        let end_index = min tab.Results.Count (start_index + DISPLAY_RESULTS_COUNT) - 1

        let panel_width = MenuRender.Width / 2
        let type_to_search = " -- TYPE TO SEARCH -- "

        panel.Write(
            ((sprintf "'%s' - %i search results" tab.Query tab.Results.Count)
                .PadRight(panel_width - type_to_search.Length + 1)
             + (if tab.SearchFocused then
                    type_to_search.ForeColor(0xFF8888)
                else
                    String.replicate type_to_search.Length " ")
             + "\n")
                .BackColor(0xFF_202020)
        )

        for i = start_index to end_index do
            let result = tab.Results.[i]

            let tag, tag_color =
                match result.Item with
                | Noun _ -> " noun ", Color.FromArgb(0xFF_ffddff)
                | Vocab v when v.LooksLikeANoun -> " noun?", Color.FromArgb(0xFF_ffddff)
                | Verb _ -> " verb ", Color.FromArgb(0xFF_ddffdd)
                | Vocab v when v.LooksLikeAVerb -> " verb?", Color.FromArgb(0xFF_ddffdd)
                | Vocab _ -> " word ", Color.White

            let tags_width = tag.Length + result.Source.WordlistName.Length + 1

            let line =
                pad_to_width(result.Item.HighlightString(), result.Item.ToString(), panel_width - tags_width)

            panel.Write(if tab.Position = i then line.BackColor(0xFF_404020) else line)
            panel.Write($" {result.Source.WordlistName} ".ForeColor(Color.LightBlue).BackColor(Color.DarkBlue))
            panel.Write((tag + "\n").ForeColor(tag_color).BackColor(0xFF_303030))

        for i = end_index + 1 to DISPLAY_RESULTS_COUNT - 1 do
            panel.Write("\n")

        Console.Write(panel.ToString())

    member this.Run() : unit =
        while state.Running do
            MenuRender.UpdateWidth()
            let right = PanelRender.Right()

            match state.RightPopup with
            | Search tab -> this.DrawSearchTab(tab, right)
            | _ -> ()

            let displayed_line =
                match state.UIContext.Buffer.ToString() with
                | "" -> state.UIContext.StatusLine
                | buffer -> buffer.ForeColor(Color.LightGreen).Bold()

            Console.Write(displayed_line.ClearRestOfLine())

            state.AddKey(Console.ReadKey(true))
            state.UIContext.Buffer.Dispatch(state.DispatchMessage, state.UIContext.BrowserKeymap)

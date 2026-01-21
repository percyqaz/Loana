open System
open Loana.CLI
open Loana.Language
open Loana.Scheduler
open Loana.Decks
open Loana.GUI

let output =
    { new IOutput with
        member this.Button(text: string, _, _, _) = Console.Write text
        member this.Write(text: string, _, _) = Console.Write text
        member this.Clear() = Console.Clear()
    }

let wordlist = Wordlist(output)
wordlist.ReadDirectory("C:/Users/percy/Desktop/Source/Loana/Wordlists")
let scheduler = CardScheduler("C:/Users/percy/Desktop/Source/Anki/Deutsch/cards.dat", output)
let decks =
    [|
        PersonalPronounsDeck() :> Deck
        ArticlesDeck()
        PossessivePronounsDeck()
        VocabDeck(wordlist)
    |]
let menu_options = decks |> Array.map (fun d -> { Name = d.Name; Menu = Func<Menu>(fun () -> d.Menu(scheduler, output, output)) })
let menu = SelectMenu(menu_options, output)
//menu.Start() |> ignore

//while menu.Next(Console.ReadLine()) do ()

//let machen = { Infinitive = Vocab.Parse "machen = to make, to do"; Separable = false; Tag = VerbTag.Transitive; Inflections = Map.empty }
//let essen = { Infinitive = Vocab.Parse "essen = to eat"; Separable = false; Tag = VerbTag.Transitive; Inflections = Map.empty }
//let abwaschen = { Infinitive = Vocab.Parse "abwaschen = to wash up"; Separable = true; Tag = VerbTag.Intransitive; Inflections = Map.empty }
//let sich_hinlegen = { Infinitive = Vocab.Parse "sich hinlegen = to lie down"; Separable = true; Tag = VerbTag.Intransitive; Inflections = Map.empty }

//printfn "%A" (VerbDownloader.extend_verb(machen, output))
//printfn "%A" (VerbDownloader.extend_verb(essen, output))
//printfn "%A" (VerbDownloader.extend_verb(abwaschen, output))
//printfn "%A" (VerbDownloader.extend_verb(sich_hinlegen, output))

App.Run(fun () ->
    let win = HtmlWindow()
    win.SetCSS (HtmlWindow.GetResource("style.css"))
    let base_html = HtmlWindow.GetResource("index.html")
    let refresh() =
        win.SetHtml (base_html.Replace("{{now}}", DateTime.UtcNow.ToString()))
    refresh()
    win.KeyDown.Add (fun k -> k.Key |> printfn "%A"; refresh())
    win
) |> ignore
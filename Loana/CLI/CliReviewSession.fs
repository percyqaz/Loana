namespace Loana.CLI

open System
open System.Drawing
open Loana.CLI

type CliCard =
    {
        Front: unit -> unit
        Back: unit -> unit
        Answer: string
    }

type CliReviewSession(cards: CliCard array) =

    let cards = ResizeArray<CliCard>(cards)

    member this.Start() =
        while cards.Count > 0 do
            let next = cards.[0]
            cards.RemoveAt(0)

            next.Front()

            if Console.ReadLine() <> next.Answer then
                Console.WriteLine(" Mistake! See below: ", Color.Black, Color.Red)
                next.Back()
                cards.Insert(min 5 cards.Count, next)
                Console.ReadLine() |> ignore
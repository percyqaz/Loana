namespace Loana.Tests

open System.IO
open System.Linq
open Loana.Data

open NUnit.Framework

module WordBankTests =

    let add_example_data (wb: WordBank, name: string) : unit =

        wb.AddWordList(
            { Group = "My Group"; WordlistName = name },
            [|
                "Löffel = spoon :m plural Löffel = spoons"
                "Messer = knife :n plural Messer = knives"
                "Gabel = fork :f plural Gabeln = forks"
                "Geschirr = dishes, tableware :n no_plural"
                "Geschirrtuch = teatowel :n"
            |]
        )

    [<Test>]
    let AddWordList_BasicUse () =
        let wb = WordBank()

        add_example_data(wb, "Cutlery")

        Assert.That(wb.Groups.Count = 1)
        Assert.That(wb.Groups.Single().Name = "My Group")
        Assert.That(wb.Groups.Single().WordlistNames.Count = 1)
        Assert.That(wb.Groups.Single().WordlistNames.Single() = "Cutlery")

        Assert.That(wb.Entries.Count = 5)

    [<Test>]
    let Duplicates_NotAddedTwice () =
        let wb = WordBank()

        add_example_data(wb, "Cutlery")
        add_example_data(wb, "Cutlery_2")

        Assert.That(wb.Groups.Count = 1)
        Assert.That(wb.Groups.Single().Name = "My Group")
        Assert.That(wb.Groups.Single().WordlistNames.Count = 2)

        Assert.That(wb.Entries.Count = 5)

    [<Test>]
    let Stream_RoundTrip () =

        let create_wb_memory_stream () =
            let wb = WordBank()
            add_example_data(wb, "Cutlery")

            let ms = new MemoryStream()
            wb.WriteToStream(ms)
            ms.Position <- 0
            ms

        use ms = create_wb_memory_stream()
        let target = WordBank()
        target.ReadFromStream(ms)

        Assert.That(target.Groups.Count = 1)
        Assert.That(target.Groups.Single().Name = "My Group")
        Assert.That(target.Groups.Single().WordlistNames.Count = 1)
        Assert.That(target.Groups.Single().WordlistNames.Single() = "Cutlery")

        Assert.That(target.Entries.Count = 5)

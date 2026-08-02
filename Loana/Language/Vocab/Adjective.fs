namespace Loana.Language

type Adjective =
    {
        Translation: Vocab
    }

    member this.Deutsch: string = this.Translation.Deutsch
    member this.English: Annotation = this.Translation.English
    member this.EnglishAlternatives: Annotation list = this.Translation.EnglishAlternatives

    member this.AsciiIdentifier: string = AsciiIdentifier.from_deutsch this.Deutsch

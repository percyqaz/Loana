namespace Loana.Tests

open Loana.Language
open NUnit.Framework

module ConjugationTests =

    [<Test>]
    let Accusative_Reflexive_Verb_Example () =
        let verb =
            Verb.FromString("sich+A über etwas+A freuen = to be happy about something :pr pa")

        let parsed = ParsedVerb.FromVerb(verb)

        Assert.That(parsed.ReflexivePronoun, Is.EqualTo(Some Case.Accusative))
        Assert.That(parsed.Objects, Is.EqualTo(Some "über etwas"))
        Assert.That(parsed.SeparableParticle, Is.EqualTo(None))
        Assert.That(parsed.Base, Is.EqualTo("freuen"))

        let ctx =
            {
                ParsedVerb = parsed
                Present = Map.ofArray [| FirstSingular, "freue" |]
                SimplePast = Map.empty
                Imperative = Map.empty
            }

        let result = ctx.Conjugate(Conjugation.Present(ConjugationPerson.FirstSingular))
        Assert.That(result, Is.EqualTo("freue mich über etwas"))

    [<Test>]
    let Separable_Reflexive_Verb_Example () =
        let verb =
            Verb.FromString("sich+A auf etwas+A vor.bereiten = to prepare for something :pr")

        let parsed = ParsedVerb.FromVerb(verb)

        Assert.That(parsed.ReflexivePronoun, Is.EqualTo(Some Case.Accusative))
        Assert.That(parsed.Objects, Is.EqualTo(Some "auf etwas"))
        Assert.That(parsed.SeparableParticle, Is.EqualTo(Some "vor"))
        Assert.That(parsed.Base, Is.EqualTo("bereiten"))

        let ctx =
            {
                ParsedVerb = parsed
                Present =
                    Map.ofArray
                        [|
                            FirstSingular, "bereite"
                            SecondSingular, "bereitest"
                            ThirdSingular, "bereitet"
                            SecondPlural, "bereitet"
                            FirstPlural, "bereiten"
                            ThirdPlural_Formal, "bereiten"
                        |]
                SimplePast = Map.empty
                Imperative = Map.empty
            }

        let result = ctx.Conjugate(Conjugation.Present(ConjugationPerson.SecondPlural))

        for conj, text in ctx.ConjugateAll(verb) do
            TestContext.Out.WriteLine(sprintf "%O: %s" conj text)

        Assert.That(result, Is.EqualTo("bereitet euch auf etwas vor"))

    [<Test>]
    let Dative_Verb_Example () =
        let verb =
            Verb.FromString("sich+D etwas+A aus.denken = to come up with something :pa")

        let parsed = ParsedVerb.FromVerb(verb)

        Assert.That(parsed.ReflexivePronoun, Is.EqualTo(Some Case.Dative))
        Assert.That(parsed.Objects, Is.EqualTo(Some "etwas"))
        Assert.That(parsed.SeparableParticle, Is.EqualTo(Some "aus"))
        Assert.That(parsed.Base, Is.EqualTo("denken"))

        let ctx =
            {
                ParsedVerb = parsed
                Present = Map.empty
                SimplePast =
                    Map.ofArray
                        [|
                            FirstSingular, "dachte"
                            SecondSingular, "dachtest"
                            SecondPlural, "dachtet"
                            ThirdSingular, "dachte"
                            FirstPlural, "dachten"
                            ThirdPlural_Formal, "dachten"
                        |]
                Imperative = Map.empty
            }

        let result = ctx.Conjugate(Conjugation.SimplePast(ConjugationPerson.SecondSingular))

        for conj, text in ctx.ConjugateAll(verb) do
            TestContext.Out.WriteLine(sprintf "%O: %s" conj text)

        Assert.That(result, Is.EqualTo("dachtest dir etwas aus"))

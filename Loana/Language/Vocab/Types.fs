namespace Loana.Language

module AsciiIdentifier =

    let from_deutsch (deutsch: string) : string =
        deutsch
            .Replace("+N", "")
            .Replace("+A", "")
            .Replace("+D", "")
            .Replace("+G", "")
            .Replace("ö", "oe")
            .Replace("ä", "ae")
            .Replace("ü", "ue")
            .Replace("ß", "ss")
            .Replace("-", "_")
            .Replace(" ", "_")

type Knowledge<'T> =
    | Unknown
    | KnownNothing
    | KnownValue of 'T

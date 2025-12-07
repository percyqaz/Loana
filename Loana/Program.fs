open Loana.Cards
open Loana.Interface

while true do

    let mode_label, mode = Quiz.pick_mode(CardPool.MODES)

    CardPool.generate_card_pool ()
    |> Seq.filter mode
    |> Seq.randomShuffle
    |> Seq.map _.Generate
    |> ResizeArray
    |> Quiz.run_quiz

// Roadmap
// Tools for mass management of tab-separated-data for nouns, verbs, adjectives
//   Plural forms of nouns will have their own entry in the noun data with a plural gender
//   Plural forms of nouns will have a back-reference to the singular form IF it exists, vice versa for singular forms
// Tools for mass generation of beautifully formatted Anki cards/HTML formatted annotations on cards instead of this console-colors annotations view
//   Acts a supplement for the existing mode and future modes added
// More learning modes:
//   Diacritics alt codes
//   Pluralise the singular form
//   Singularise the plural
//   Gender the noun (inc. telling if something is in the plural gender)
# Repository context

Loana is an F# German-learning application. The solution contains a domain/data library (`Loana`), a terminal application (`Loana.Desktop`), a mobile app (`Loana.Mobile`), and tests (`Loana.Tests`). Check the user's requested scope before touching projects; prior architecture inspection focused on `Loana` and `Loana.Desktop` only.

## Project relationship

`Loana.Desktop` references `Loana`. The library defines the language and study domain, wordlist and review data, and vocabulary/verb card logic. The desktop executable owns terminal input/rendering and the user-facing flows that use those library types. In broad terms: terminal interaction -> desktop menu/session/browser/quizzes -> library models, word banks, and review schedule.

## `Loana` source map

- `Language/`: core German-learning types and rules: gender, cases, persons, vocabulary annotations, nouns, verbs, conjugations, and German/English declension helpers.
- `Data/`: app data and persistence. `WordBank.fs` parses, validates, groups, and writes wordlists; `ReviewSchedule.fs` stores spaced-review state; `VerbBank.fs` persists conjugations; `LoanaState.fs` constructs the combined application state; `Sync.fs` handles schedule/wordlist network sync; `ConjugationDownloader.fs` retrieves missing conjugations from Reverso.
- `Vocab/`: vocabulary flashcard construction and available/possible card tiers, filtering, and missing-information chores.
- `Verbs/`: verb-conjugation card identity and cache of verb/tenses for practice.

The library project compiles explicit F# source files in order in `Loana.fsproj`. It is more than pure domain types: it also owns storage formats and application data behavior.

## `Loana.Desktop` source map

- `Program.fs`: executable entry point; reads the data directory from the user's `.loana` config, creates `LoanaState`, displays wordlist errors, starts the menu, and saves the schedule at exit.
- `CLI/`: terminal rendering, keymaps, command/text buffers, and shared UI context.
- `Menu/`: dashboard, menu state, and command routing for study, browser, and quiz flows.
- `Study/`: card display/session handling and card sources that apply review outcomes to the library scheduler.
- `Browser/`: terminal wordlist/group/search views and their commands.
- `Quizzes/`: grammar practice flows (including articles and pronouns), with quiz sessions using the shared review scheduler.

This is a console/terminal app, despite the project name. Like the library, its F# files are explicitly ordered in `Loana.Desktop.fsproj`.

## Persistent data notes

The desktop app's `.loana` file stores the configured data-directory path. That directory contains the user's wordlists, `cards.dat` for review scheduling, and `verbs.verblist` for saved conjugations. Avoid treating generated `bin/` and `obj/` output as source or editing it to implement features.


namespace Loana.Desktop.CLI

type UIContext =
    {
        Buffer: CommandBuffer
        GlobalKeymap: Keymap
        MenuKeymap: Keymap
        StudyKeymap: Keymap
        BrowserKeymap: Keymap
        mutable StatusLine: string
    // Use stack to store current screen rather than represent it here
    }

    static member DefaultMenuKeymap() : Keymap =
        let keymap = Keymap()
        keymap.AliasCommand("<Esc>", "exit")
        keymap.AliasCommand("j", "down")
        keymap.AliasCommand("k", "up")
        keymap.AliasCommand("<Enter>", "stats")
        keymap.AliasCommand("r", "review")
        keymap.AliasCommand("l", "learn")
        keymap.AliasCommand("a", "ahead")
        keymap.AliasCommand("c", "chores")
        keymap.AliasCommand("f", "filter")
        keymap.AliasCommand("-", "batch_down")
        keymap.AliasCommand("=", "batch_up")
        keymap.AliasCommand("s", "sync")
        keymap.AliasCommand("b", "browse")
        keymap.Alias("<Down>", "j")
        keymap.Alias("<Up>", "k")
        keymap

    static member DefaultStudyKeymap() : Keymap =
        let keymap = Keymap()
        keymap.AliasCommand("<Esc>", "exit")
        keymap.AliasCommand(" ", "reveal")
        keymap.AliasCommand("z", "forgot")
        keymap.AliasCommand(",", "bad")
        keymap.AliasCommand(".", "ok")
        keymap.AliasCommand("/", "good")
        keymap

    static member DefaultBrowserKeymap() : Keymap =
        let keymap = Keymap()
        keymap.AliasCommand("<Esc>", "exit")
        keymap.AliasCommand("h", "left")
        keymap.AliasCommand("j", "down")
        keymap.AliasCommand("k", "up")
        keymap.AliasCommand("l", "right")
        keymap.AliasCommand("<A-j>", "move_down")
        keymap.AliasCommand("<A-k>", "move_up")
        keymap.AliasCommand("<Tab>", "search")
        keymap.AliasCommand("<Enter>", "select")
        keymap.Alias("<Left>", "h")
        keymap.Alias("<Down>", "j")
        keymap.Alias("<Up>", "k")
        keymap.Alias("<Right>", "l")
        keymap.Alias("<A-Down>", "<A-j>")
        keymap.Alias("<A-Up>", "<A-k>")
        keymap

    static member Create() : UIContext =
        {
            Buffer = CommandBuffer()
            GlobalKeymap = Keymap()
            MenuKeymap = UIContext.DefaultMenuKeymap()
            StudyKeymap = UIContext.DefaultStudyKeymap()
            BrowserKeymap = UIContext.DefaultBrowserKeymap()
            StatusLine = ""
        }

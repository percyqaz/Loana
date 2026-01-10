using System.Linq;
using System.Collections.Generic;
using Avalonia.Controls;
using Avalonia.Media;
using Loana.Scheduler;
using Loana.Interface;
using Loana.Decks;
using Loana.Database;

namespace Loana.GUI;

public partial class MainWindow : Window
{
    private readonly Nouns _nouns = new("C:/users/percy/Desktop/Source/Anki/Deutsch/nouns.dat");

    public MainWindow()
    {
        InitializeComponent();

        Input.Focus();

        var log = this.FindControl<Terminal>("Log")!;
        var display = this.FindControl<Terminal>("MainDisplay")!;

        _nouns.Validate(log);
        CardScheduler scheduler = new(log);

        Deck[] decks = [new PersonalPronounsDeck(), new ArticlesDeck(), new PossessivePronounsDeck()];
        List<SelectMenuOption> menuOptions =
            [.. decks.Select(deck => new SelectMenuOption(
                deck.Name,
                () => deck.Menu(scheduler, log, display)
            ))];
        menuOptions.Add(new SelectMenuOption("Browse Nouns", () => NounBrowser.create(_nouns, display)));
        var menu = new SelectMenu(
            [.. menuOptions],
            display
        );
        log.WriteLine("Welcome to Loana!", Brushes.Wheat);
        menu.Start();

        Input.KeyDown += (sender, e) =>
        {
            if (e.Key == Avalonia.Input.Key.Enter)
            {
                string command = Input.Text ?? "";
                Input.Text = "";
                if (!menu.Next(command))
                {
                    Close();
                }
            }
        };

        display.OnButtonClicked += command =>
        {
            if (!menu.Next(command))
            {
                Close();
            }
        };
    }

    protected override void OnClosing(WindowClosingEventArgs e)
    {
        _nouns.Save();
    }
}
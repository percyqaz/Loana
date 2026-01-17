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
    public MainWindow()
    {
        InitializeComponent();

        Input.Focus();

        var log = this.FindControl<Terminal>("Log")!;
        var display = this.FindControl<Terminal>("MainDisplay")!;

        Wordlist wordlist = new(log);
        wordlist.ReadDirectory("C:/Users/percy/Desktop/Source/Loana/Wordlists");
        CardScheduler scheduler = new("C:/Users/percy/Desktop/Source/Anki/Deutsch/cards.dat", log);

        Deck[] decks = [new PersonalPronounsDeck(), new ArticlesDeck(), new PossessivePronounsDeck()];
        List<SelectMenuOption> menuOptions =
            [.. decks.Select(deck => new SelectMenuOption(
                deck.Name,
                () => deck.Menu(scheduler, log, display)
            ))];
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
            else if (e.Key == Avalonia.Input.Key.Tab)
            {
                Split.IsPaneOpen = !Split.IsPaneOpen;
                e.Handled = true;
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
}
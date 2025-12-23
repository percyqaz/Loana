using System.Linq;
using Avalonia.Controls;
using Avalonia.Media;
using Loana.Scheduler;
using Loana.Interface;
using Loana.Decks;

namespace Loana.GUI;

public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();

        Input.Focus();

        var log = this.FindControl<Terminal>("Log")!;
        var display = this.FindControl<Terminal>("MainDisplay")!;

        CardScheduler scheduler = new(log);

        Deck[] decks = [new PersonalPronounsDeck(), new ArticlesDeck(), new PossessivePronounsDeck()];
        var menu = new SelectMenu(
            [.. decks.Select(deck => new SelectMenuOption(
                deck.Name,
                () => deck.Menu(scheduler, log, display)
            ))],
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
    }
}
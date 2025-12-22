using Avalonia.Controls;
using Avalonia.Media;
using Loana.Scheduler;
using Loana.Interface;
using Loana.Decks;
using System.Linq;

namespace Loana.GUI;

public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();

        Input.Focus();

        var log = this.FindControl<Terminal>("Log")!;
        var display = this.FindControl<Terminal>("MainDisplay")!;

        log.WriteLine("Welcome to Loana!", Brushes.Wheat);

        Deck[] decks = [new PersonalPronounsDeck(), new ArticlesDeck(), new PossessivePronounsDeck()];
        CardScheduler scheduler = new(log);
        var menu = new SelectMenu(
            [.. decks.Select(deck => new SelectMenuOption(
                deck.Name,
                () =>
                {
                    var stack = CardStack.Build(deck.Build(scheduler), true, 50);
                    return new ReviewSession(stack, log, display);
                }
            ))],
            display
        );
        menu.Start();

        Input.KeyDown += (sender, e) =>
        {
            if (e.Key == Avalonia.Input.Key.Enter)
            {
                string command = Input.Text ?? "";
                Input.Text = "";
                menu.Next(command);
            }
        };
    }
}
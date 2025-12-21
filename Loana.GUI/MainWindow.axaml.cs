using Avalonia.Controls;
using Avalonia.Media;
using Loana.Scheduler;
using Loana.Interface;
using Loana.Cards;

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

        Deck[] decks = [new Pronouns(), new Articles(), new PossessivePronouns()];
        CardScheduler scheduler = new CardScheduler(log);
        ReviewSession? session = null;
        var menu = MenuContext.Create(
            decks,
            x => x.Name,
            deck =>
            {
                var stack = CardStack.Build(deck.Build(scheduler), true, 50);
                session = ReviewSession.Create(stack, log, display);
                if (!session.Next(""))
                {
                    session = null;
                }
            },
            display
        );
        menu.Draw();

        Input.KeyDown += (sender, e) =>
        {
            if (e.Key == Avalonia.Input.Key.Enter)
            {
                string command = Input.Text ?? "";
                Input.Text = "";
                if (session is not null)
                {
                    if (!session.Next(command))
                    {
                        session = null;
                        menu.Draw();
                    }
                }
                else
                {
                    menu.Next(command);
                }
            }
        };
    }
}
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

        var log = this.FindControl<Terminal>("Log")!;
        var display = this.FindControl<Terminal>("MainDisplay")!;

        log.WriteLine("Welcome to Loana!", Brushes.Wheat);

        CardScheduler scheduler = new CardScheduler(log);
        ReviewSession? session = null;
        var menu = MenuContext.CreateDeckPicker(
            filter =>
            {
                var deck = CardStack.Build(CardPool.build_from_filter(filter, CardSpacingRule.Familiarise, scheduler), true, 50);
                session = ReviewSession.Create(deck, log, display);
                session.Next("");
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
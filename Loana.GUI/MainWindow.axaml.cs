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

        CardSchedule scheduler = new CardSchedule(CardSpacingRule.Familiarise, log);
        ReviewSession? session = null;
        var menu = MenuContext.CreateModePicker(
            mode =>
            {
                var deck = CardStack.Build(CardPool.build_from_mode(mode, scheduler), true, 50);
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
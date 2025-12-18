using Avalonia.Controls;
using Avalonia.Media;
using Loana.Cards;
using System.Linq;

namespace Loana.GUI;

public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();

        var terminal = this.FindControl<Terminal>("Terminal")!;

        var cards = Interface.Quiz.example();
        Interface.Quiz.display_card(cards.First(), terminal);

        Input.KeyDown += (sender, e) =>
        {
            if (e.Key == Avalonia.Input.Key.Enter)
            {
                string command = Input.Text ?? "";
                Input.Text = "";
                terminal.WriteLine(command, Brushes.Wheat);
            }
        };
    }
}
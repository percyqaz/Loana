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

        terminal.WriteLine("Welcome to Loana!", Brushes.Wheat);
        var quiz = Interface.QuizContext.CreateExample(terminal);

        Input.KeyDown += (sender, e) =>
        {
            if (e.Key == Avalonia.Input.Key.Enter)
            {
                string command = Input.Text ?? "";
                Input.Text = "";
                quiz.Next(command);
            }
        };
    }
}
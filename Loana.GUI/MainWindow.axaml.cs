using Avalonia.Controls;
using Avalonia.Media;
using Loana.Interface;
using System.Linq;

namespace Loana.GUI;

public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();

        var terminal = this.FindControl<Terminal>("Terminal")!;

        terminal.WriteLine("Welcome to Loana!", Brushes.Wheat);

        QuizContext? quiz = null;
        var menu = MenuContext.CreateModePicker(mode => { quiz = QuizContext.CreateFromMode(mode, terminal); quiz.Next(""); }, terminal);
        menu.Draw();

        Input.KeyDown += (sender, e) =>
        {
            if (e.Key == Avalonia.Input.Key.Enter)
            {
                string command = Input.Text ?? "";
                Input.Text = "";
                if (quiz is not null)
                {
                    if (!quiz.Next(command))
                    {
                        quiz = null;
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
using Avalonia.Controls;
using Avalonia.Media;
using Loana.Interface;

namespace Loana.GUI;

public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();

        var log = this.FindControl<Terminal>("Log")!;
        var display = this.FindControl<Terminal>("MainDisplay")!;

        log.WriteLine("Welcome to Loana!", Brushes.Wheat);

        QuizContext? quiz = null;
        var menu = MenuContext.CreateModePicker(mode => { quiz = QuizContext.CreateFromMode(mode, log, display); quiz.Next(""); }, display);
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
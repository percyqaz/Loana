namespace Loana.Mobile.Pages
{
    public partial class LearnPage : ContentPage
    {
        public LearnPage(LearnPageModel model)
        {
            InitializeComponent();
            BindingContext = model;
        }
    }
}
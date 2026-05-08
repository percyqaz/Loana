namespace Loana.Mobile.Pages
{
    public partial class LearnPage : ContentPage
    {
        private LearnPageModel _model;

        public LearnPage(LearnPageModel model)
        {
            InitializeComponent();
            _model = model;
            BindingContext = model;
        }
    }
}
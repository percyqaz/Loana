namespace Loana.Mobile.Pages
{
    public partial class ReviewPage : ContentPage
    {
        private ReviewPageModel _model;

        public ReviewPage(ReviewPageModel model)
        {
            InitializeComponent();
            _model = model;
            BindingContext = model;
        }
    }
}
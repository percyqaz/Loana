namespace Loana.Mobile.Pages
{
    public partial class ReviewPage : ContentPage
    {
        public ReviewPage(ReviewPageModel model)
        {
            InitializeComponent();
            BindingContext = model;
        }
    }
}
namespace Loana.Mobile.Pages
{
    public partial class ReviewPage : ContentPage
    {
        public ReviewPage(ReviewPageModel model)
        {
            InitializeComponent();
            BindingContext = model;
        }

        private void SwipeGestureRecognizer_Swiped(object sender, SwipedEventArgs e)
        {
        }
    }
}
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

        private void OnSwiped(object? sender, SwipedEventArgs e)
        {
            switch (e.Direction)
            {
                case SwipeDirection.Left:
                    _model.OnSwipedLeft().GetAwaiter().GetResult();
                    break;

                case SwipeDirection.Down:
                    _model.OnSwipedDown().GetAwaiter().GetResult();
                    break;

                case SwipeDirection.Up:
                    _model.OnSwipedUp().GetAwaiter().GetResult();
                    break;

                case SwipeDirection.Right:
                    _model.OnSwipedRight().GetAwaiter().GetResult();
                    break;

                default:
                    return;
            }
        }

        private void OnTapped(object? sender, TappedEventArgs e)
        {
            _model.OnTapped().GetAwaiter().GetResult();
        }
    }
}
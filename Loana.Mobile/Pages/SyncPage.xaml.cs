namespace Loana.Mobile.Pages
{
    public partial class SyncPage : ContentPage
    {
        public SyncPage(SyncPageModel model)
        {
            InitializeComponent();
            BindingContext = model;
        }
    }
}
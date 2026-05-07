using Loana.Mobile.Models;
using Loana.Mobile.PageModels;

namespace Loana.Mobile.Pages
{
    public partial class MainPage : ContentPage
    {
        public MainPage(MainPageModel model)
        {
            InitializeComponent();
            BindingContext = model;
        }
    }
}
namespace Loana.Mobile.PageModels;

public partial class MainPageModel(ModalErrorHandler errorHandler) : ObservableObject
{
    private bool _isNavigatedTo = true;
    private readonly ModalErrorHandler _errorHandler = errorHandler;

    [ObservableProperty]
    private bool _isRefreshing;

    [RelayCommand]
    private async Task Refresh()
    {
    }

    [RelayCommand]
    private void NavigatedTo() =>
        _isNavigatedTo = true;

    [RelayCommand]
    private void NavigatedFrom() =>
        _isNavigatedTo = false;

    [RelayCommand]
    private async Task Appearing()
    {
    }
}
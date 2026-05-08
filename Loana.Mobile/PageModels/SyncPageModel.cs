namespace Loana.Mobile.PageModels;

public partial class SyncPageModel(LoanaRepository _loanaRepository) : ObservableObject
{
    [ObservableProperty]
    private string _address = "10.0.2.2";

    [RelayCommand]
    private async Task Sync()
    {
        _loanaRepository.Resync(Address);
    }
}
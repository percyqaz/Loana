namespace Loana.Mobile.PageModels;

public partial class SyncPageModel(LoanaRepository _loanaRepository) : ObservableObject
{
    [ObservableProperty]
    private string _address = "192.168.0.69";

    [ObservableProperty]
    private string _status = "";

    [RelayCommand]
    private async Task SyncProgress()
    {
        if (Address == "") return;
        Status = "";
        _loanaRepository.SyncProgress(Address);
        Status = "Synced progress!";
    }

    [RelayCommand]
    private async Task DownloadWords()
    {
        if (Address == "") return;
        Status = "";
        _loanaRepository.DownloadWords(Address);
        Status = "Downloaded wordlists!";
    }
}
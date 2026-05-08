using Loana.Mobile.Models;
using System.Collections.ObjectModel;

namespace Loana.Mobile.PageModels;

public partial class DashboardPageModel(LoanaRepository loanaRepository) : ObservableObject
{
    private readonly LoanaRepository _loanaRepository = loanaRepository;

    [ObservableProperty]
    private ObservableCollection<VocabListGroup> _groups = [];

    [ObservableProperty]
    private VocabAll _allCards = new(0, 0, 0, 0);

    private async Task LoadData()
    {
        Groups = new ObservableCollection<VocabListGroup>(await _loanaRepository.ListAsync());
        AllCards = await _loanaRepository.GetAsync();
    }

    [RelayCommand]
    private Task Appearing()
        => LoadData();

    [ObservableProperty]
    private bool _isRefreshing;

    [RelayCommand]
    private async Task Refresh()
    {
        IsRefreshing = true;
        await LoadData();
        IsRefreshing = false;
    }

    [RelayCommand]
    private async Task Learn() => await Shell.Current.GoToAsync("review"); // todo: learn

    [RelayCommand]
    private async Task Review() => await Shell.Current.GoToAsync("review");
}
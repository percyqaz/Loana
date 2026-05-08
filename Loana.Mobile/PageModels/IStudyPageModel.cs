using Loana.Language;

namespace Loana.Mobile.PageModels;

public abstract partial class IStudyPageModel : ObservableObject
{
    public readonly List<Card> Cards = [];

    [ObservableProperty]
    private int _remainingCards = 0;

    [ObservableProperty]
    private Card? _currentCard;

    [ObservableProperty]
    private bool _showBack;

    protected void NextCard()
    {
        RemainingCards = Cards.Count;
        if (RemainingCards > 0)
        {
            ShowBack = false;
            CurrentCard = Cards[0];
            Cards.RemoveAt(0);
        }
        else
        {
            ShowBack = false;
        }
    }

    [RelayCommand]
    public async Task Tapped()
    {
        if (CurrentCard is null || ShowBack) return;
        ShowBack = true;
    }

    [RelayCommand]
    public abstract Task SwipedLeft();

    [RelayCommand]
    public abstract Task SwipedDown();

    [RelayCommand]
    public abstract Task SwipedUp();

    [RelayCommand]
    public abstract Task SwipedRight();
}
using Loana.Language;

namespace Loana.Mobile.PageModels;

public partial class ReviewPageModel : ObservableObject
{
    private readonly LoanaRepository _loanaRepository;
    private readonly List<Card> _cards;

    [ObservableProperty]
    private int _remainingCards = 0;

    [ObservableProperty]
    private Card? _currentCard;

    [ObservableProperty]
    private bool _showBack;

    public ReviewPageModel(LoanaRepository loanaRepository)
    {
        _loanaRepository = loanaRepository;
        _cards = _loanaRepository.Review();
        _remainingCards = _cards.Count;
        if (_remainingCards > 0)
        {
            _showBack = false;
            _currentCard = _cards[0];
            _cards.RemoveAt(0);
        }
    }

    public void NextCard()
    {
        RemainingCards = _cards.Count;
        if (RemainingCards > 0)
        {
            ShowBack = false;
            CurrentCard = _cards[0];
            _cards.RemoveAt(0);
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
    public async Task SwipedLeft()
    {
        if (CurrentCard is null || !ShowBack) return;
        _loanaRepository.Scheduler.Promote(CurrentCard);
        NextCard();
    }

    [RelayCommand]
    public async Task SwipedDown()
    {
        if (CurrentCard is null || !ShowBack) return;
        _loanaRepository.Scheduler.Keep(CurrentCard);
        NextCard();
    }

    [RelayCommand]
    public async Task SwipedUp()
    {
        if (CurrentCard is null || !ShowBack) return;
        _loanaRepository.Scheduler.Forget(CurrentCard);
        NextCard();
    }

    [RelayCommand]
    public async Task SwipedRight()
    {
        if (CurrentCard is null || !ShowBack) return;
        _loanaRepository.Scheduler.Demote(CurrentCard);
        NextCard();
    }
}
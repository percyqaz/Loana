using Loana.Language;

namespace Loana.Mobile.PageModels;

public partial class ReviewPageModel : ObservableObject
{
    private readonly LoanaRepository _loanaRepository;
    private readonly List<Card> _cards;

    public enum ReviewPageState
    {
        FrontSide = 0,
        BackSide = 1,
        Complete = 2
    }

    [ObservableProperty]
    private int _remainingCards = 0;

    [ObservableProperty]
    private Card? _currentCard;

    [ObservableProperty]
    private bool _showBack;

    [ObservableProperty]
    private ReviewPageState _state = ReviewPageState.Complete;

    public ReviewPageModel(LoanaRepository loanaRepository)
    {
        _loanaRepository = loanaRepository;
        _cards = _loanaRepository.Review();
        _remainingCards = _cards.Count;
        if (_remainingCards > 0)
        {
            _state = ReviewPageState.FrontSide;
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
            State = ReviewPageState.FrontSide;
            ShowBack = false;
            CurrentCard = _cards[0];
            _cards.RemoveAt(0);
        }
        else
        {
            State = ReviewPageState.Complete;
            ShowBack = false;
        }
    }

    public async Task OnTapped()
    {
        if (CurrentCard is null || State != ReviewPageState.FrontSide) return;
        State = ReviewPageState.BackSide;
        ShowBack = true;
    }

    public async Task OnSwipedLeft()
    {
        if (CurrentCard is null || State != ReviewPageState.BackSide) return;
        _loanaRepository.Scheduler.Promote(CurrentCard);
        NextCard();
    }

    public async Task OnSwipedDown()
    {
        if (CurrentCard is null || State != ReviewPageState.BackSide) return;
        _loanaRepository.Scheduler.Keep(CurrentCard);
        NextCard();
    }

    public async Task OnSwipedUp()
    {
        if (CurrentCard is null || State != ReviewPageState.BackSide) return;
        _loanaRepository.Scheduler.Forget(CurrentCard);
        NextCard();
    }

    public async Task OnSwipedRight()
    {
        if (CurrentCard is null || State != ReviewPageState.BackSide) return;
        _loanaRepository.Scheduler.Demote(CurrentCard);
        NextCard();
    }
}
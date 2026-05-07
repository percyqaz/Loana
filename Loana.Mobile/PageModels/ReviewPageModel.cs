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
    private ReviewPageState _state = ReviewPageState.Complete;

    public ReviewPageModel(LoanaRepository loanaRepository)
    {
        _loanaRepository = loanaRepository;
        _cards = _loanaRepository.Review();
        _remainingCards = _cards.Count;
        if (_remainingCards > 0)
        {
            _state = ReviewPageState.FrontSide;
            _currentCard = _cards[0];
        }
    }

    public async Task OnSwipedLeft(object sender, SwipedEventArgs e)
    {
        await Shell.Current.GoToAsync("main");
    }
}
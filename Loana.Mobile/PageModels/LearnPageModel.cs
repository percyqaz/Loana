namespace Loana.Mobile.PageModels;

public class LearnPageModel : IStudyPageModel
{
    private readonly LoanaRepository _loanaRepository;

    public LearnPageModel(LoanaRepository loanaRepository)
    {
        _loanaRepository = loanaRepository;
        Cards.AddRange(_loanaRepository.Learn());
        NextCard();
    }

    public override async Task SwipedLeft()
    {
        if (CurrentCard is null || !ShowBack) return;
        _loanaRepository.Scheduler.Learn(CurrentCard);
        NextCard();
    }

    public override async Task SwipedDown()
    {
        if (CurrentCard is null || !ShowBack) return;
        Cards.Add(CurrentCard);
        NextCard();
    }

    public override async Task SwipedRight()
    {
        if (CurrentCard is null || !ShowBack) return;
        Cards.Insert(Math.Min(Cards.Count, 4), CurrentCard);
        NextCard();
    }

    public override async Task SwipedUp()
    {
        if (CurrentCard is null || !ShowBack) return;
        _loanaRepository.Scheduler.Bury(CurrentCard.Key);
        NextCard();
    }
}
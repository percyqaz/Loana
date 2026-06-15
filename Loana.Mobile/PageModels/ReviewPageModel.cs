namespace Loana.Mobile.PageModels;

public partial class ReviewPageModel : IStudyPageModel
{
    private readonly LoanaRepository _loanaRepository;

    public ReviewPageModel(LoanaRepository loanaRepository)
    {
        _loanaRepository = loanaRepository;
        Cards.AddRange(_loanaRepository.Review());
        NextCard();
    }

    public override async Task SwipedLeft()
    {
        if (CurrentCard is null || !ShowBack) return;
        _loanaRepository.Scheduler.Promote(CurrentCard);
        NextCard();
    }

    public override async Task SwipedDown()
    {
        if (CurrentCard is null || !ShowBack) return;
        _loanaRepository.Scheduler.Keep(CurrentCard);
        NextCard();
    }

    public override async Task SwipedRight()
    {
        if (CurrentCard is null || !ShowBack) return;
        _loanaRepository.Scheduler.Demote(CurrentCard);
        NextCard();
    }

    public override async Task SwipedUp()
    {
        if (CurrentCard is null || !ShowBack) return;
        _loanaRepository.Scheduler.Forget(CurrentCard);
        NextCard();
    }
}
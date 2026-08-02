using Loana.Data;
using Loana.Language;
using Loana.Mobile.Models;
using Loana.Vocab;
using Microsoft.Extensions.Logging;

namespace Loana.Mobile.Data;

/// <summary>
///     Repository class for managing tags in the database.
/// </summary>
public class LoanaRepository
{
    private readonly ILogger _logger;
    private readonly LoanaState _state;
    private readonly VocabDeck _vocab;

    public LoanaRepository(ILogger<LoanaRepository> logger)
    {
        _logger = logger;
        _state = LoanaState.Create(FileSystem.AppDataDirectory);
        _vocab = new VocabDeck(_state.Scheduler, _state.Words);
        _logger.LogInformation("Loaded {WordCount} word entries", _state.Words.Entries.Count);
    }

    public ReviewSchedule Scheduler => _state.Scheduler;
    public WordBank Words => _state.Words;

    public void DownloadWords(string address)
    {
        Sync.connect_wordlists(_state, address);
        _logger.LogInformation("Loaded {WordCount} word entries", _state.Words.Entries.Count);
    }

    public void SyncProgress(string address)
    {
        Sync.connect_schedule(_state, address);
    }

    public async Task<List<VocabListGroup>> ListAsync()
    {
        var now = DateTimeOffset.UtcNow.ToUnixTimeSeconds();
        List<VocabListGroup> groups = [];
        foreach (var group in Words.Groups)
        {
            var g_available = _vocab.AvailableCards(group.WordlistNames);
            var g_learning = _vocab.LearningCards(g_available);
            var g_due = _vocab.DueReviewCards(g_available, now);
            var g_ahead = _vocab.AheadReviewCards(g_available, now);

            List<VocabList> lists = [];

            foreach (var list in group.WordlistNames)
            {
                var available = _vocab.AvailableCards([list]);
                var learning = _vocab.LearningCards(available);
                var due = _vocab.DueReviewCards(available, now);
                var ahead = _vocab.AheadReviewCards(available, now);

                lists.Add(new VocabList(list, learning.Count(), due.Count(), ahead.Count(), available.Count()));
            }

            groups.Add(new VocabListGroup(group.Name, g_learning.Count(), g_due.Count(), g_ahead.Count(),
                g_available.Count(), lists));
        }

        return groups;
    }

    public async Task<VocabAll> GetAsync()
    {
        var now = DateTimeOffset.UtcNow.ToUnixTimeSeconds();
        var available = _vocab.AvailableCards([]);
        var learning = _vocab.LearningCards(available);
        var due = _vocab.DueReviewCards(available, now);
        var ahead = _vocab.AheadReviewCards(available, now);

        return new VocabAll(learning.Count(), due.Count(), ahead.Count(), available.Count());
    }

    public List<Card> Review()
    {
        var now = DateTimeOffset.UtcNow.ToUnixTimeSeconds();
        return _vocab.DueReviewCards(_vocab.AvailableCards([]), now)
            .Take(50)
            .Shuffle()
            .ToList();
    }

    public List<Card> Learn()
    {
        return _vocab.LearningCards(_vocab.AvailableCards([]))
            .Take(20)
            .Shuffle()
            .ToList();
    }
}
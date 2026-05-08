using Loana.Data;
using Loana.Language;
using Loana.Mobile.Models;
using Loana.Vocab;
using Microsoft.Extensions.Logging;

namespace Loana.Mobile.Data;

/// <summary>
/// Repository class for managing tags in the database.
/// </summary>
public class LoanaRepository
{
    private readonly ILogger _logger;
    private readonly ReviewSchedule _scheduler;
    private readonly WordBank _words;
    private readonly VocabDeck _vocab;

    public LoanaRepository(ILogger<TagRepository> logger)
    {
        _logger = logger;
        _scheduler = new ReviewSchedule(Path.Combine(FileSystem.AppDataDirectory, "cards.dat"));
        _words = WordBank.FromDirectory(FileSystem.AppDataDirectory);
        _logger.LogInformation("Loaded {WordCount} word entries", _words.Entries.Count);
        _vocab = new VocabDeck(_scheduler, _words);
    }

    public void Resync(string address)
    {
        Sync.connect(_scheduler, _words, address);
        _logger.LogInformation("Loaded {WordCount} word entries", _words.Entries.Count);
    }

    public async Task<List<VocabListGroup>> ListAsync()
    {
        var now = DateTimeOffset.UtcNow.ToUnixTimeSeconds();
        List<VocabListGroup> groups = [];
        foreach (var group in _words.Groups)
        {
            var g_available = _vocab.AvailableCards(group.Lists);
            var g_learning = _vocab.LearningCards(g_available);
            var g_due = _vocab.DueReviewCards(g_available, now);
            var g_ahead = _vocab.AheadReviewCards(g_available, now);

            List<VocabList> lists = [];

            foreach (var list in group.Lists)
            {
                var available = _vocab.AvailableCards([list]);
                var learning = _vocab.LearningCards(available);
                var due = _vocab.DueReviewCards(available, now);
                var ahead = _vocab.AheadReviewCards(available, now);

                lists.Add(new VocabList(list, learning.Count(), due.Count(), ahead.Count(), available.Count()));
            }

            groups.Add(new VocabListGroup(group.Name, g_learning.Count(), g_due.Count(), g_ahead.Count(), g_available.Count(), lists));
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

    public ReviewSchedule Scheduler => _scheduler;
}
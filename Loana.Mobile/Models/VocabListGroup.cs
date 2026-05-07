namespace Loana.Mobile.Models;

public record VocabListGroup(string Name, int Learning, int Due, int Ahead, int Available, List<VocabList> Lists);
using System.Globalization;
using Loana.Language;

namespace Loana.Mobile.Pages.Controls;

public class DeutschConverter : IValueConverter
{
    public object Convert(object? value, Type targetType, object? parameter, CultureInfo culture) => (CardType)value! switch
    {
        CardType.RecogniseDE x => x.v.Deutsch,
        CardType.RecallDE x => x.v.Deutsch,
        CardType.RecogniseArticleDE x => x.n.Deutsch,
        CardType.RecallArticleDE x => x.n.Deutsch,
        _ => throw new ArgumentOutOfRangeException()
    };

    public object ConvertBack(object? value, Type targetType, object? parameter, CultureInfo culture)
    {
        throw new NotImplementedException();
    }
}

public class EnglishConverter : IValueConverter
{
    public object Convert(object? value, Type targetType, object? parameter, CultureInfo culture) => (CardType)value! switch
    {
        CardType.RecogniseDE x => (new List<Annotation> { x.v.English }).Concat(x.v.EnglishAlternatives),
        CardType.RecallDE x => (new List<Annotation> { x.v.English }).Concat(x.v.EnglishAlternatives),
        CardType.RecogniseArticleDE x => (new List<Annotation> { x.n.English }).Concat(x.n.EnglishAlternatives),
        CardType.RecallArticleDE x => (new List<Annotation> { x.n.English }).Concat(x.n.EnglishAlternatives),
        _ => throw new ArgumentOutOfRangeException()
    };

    public object ConvertBack(object? value, Type targetType, object? parameter, CultureInfo culture)
    {
        throw new NotImplementedException();
    }
}

public class EnglishRowConverter : IValueConverter
{
    public object Convert(object? value, Type targetType, object? parameter, CultureInfo culture) => (CardType)value! switch
    {
        CardType.RecogniseDE => 1,
        CardType.RecallDE => 0,
        CardType.RecogniseArticleDE => 1,
        CardType.RecallArticleDE => 0,
        _ => throw new ArgumentOutOfRangeException()
    };

    public object ConvertBack(object? value, Type targetType, object? parameter, CultureInfo culture)
    {
        throw new NotImplementedException();
    }
}

public class DeutschRowConverter : IValueConverter
{
    public object Convert(object? value, Type targetType, object? parameter, CultureInfo culture) => (CardType)value! switch
    {
        CardType.RecogniseDE => 0,
        CardType.RecallDE => 1,
        CardType.RecogniseArticleDE => 0,
        CardType.RecallArticleDE => 1,
        _ => throw new ArgumentOutOfRangeException()
    };

    public object ConvertBack(object? value, Type targetType, object? parameter, CultureInfo culture)
    {
        throw new NotImplementedException();
    }
}

public class MasculineConverter : IValueConverter
{
    public object Convert(object? value, Type targetType, object? parameter, CultureInfo culture) => (CardType)value! switch
    {
        CardType.RecogniseDE _ => false,
        CardType.RecallDE _ => false,
        CardType.RecogniseArticleDE x => x.n.Guts.Gender.IsMasculine,
        CardType.RecallArticleDE x => x.n.Guts.Gender.IsMasculine,
        _ => throw new ArgumentOutOfRangeException()
    };

    public object ConvertBack(object? value, Type targetType, object? parameter, CultureInfo culture)
    {
        throw new NotImplementedException();
    }
}

public class FeminineConverter : IValueConverter
{
    public object Convert(object? value, Type targetType, object? parameter, CultureInfo culture) => (CardType)value! switch
    {
        CardType.RecogniseDE _ => false,
        CardType.RecallDE _ => false,
        CardType.RecogniseArticleDE x => x.n.Guts.Gender.IsFeminine,
        CardType.RecallArticleDE x => x.n.Guts.Gender.IsFeminine,
        _ => throw new ArgumentOutOfRangeException()
    };

    public object ConvertBack(object? value, Type targetType, object? parameter, CultureInfo culture)
    {
        throw new NotImplementedException();
    }
}

public class NeuterConverter : IValueConverter
{
    public object Convert(object? value, Type targetType, object? parameter, CultureInfo culture) => (CardType)value! switch
    {
        CardType.RecogniseDE _ => false,
        CardType.RecallDE _ => false,
        CardType.RecogniseArticleDE x => x.n.Guts.Gender.IsNeuter,
        CardType.RecallArticleDE x => x.n.Guts.Gender.IsNeuter,
        _ => throw new ArgumentOutOfRangeException()
    };

    public object ConvertBack(object? value, Type targetType, object? parameter, CultureInfo culture)
    {
        throw new NotImplementedException();
    }
}

public class PluralConverter : IValueConverter
{
    public object Convert(object? value, Type targetType, object? parameter, CultureInfo culture) => (CardType)value! switch
    {
        CardType.RecogniseDE _ => false,
        CardType.RecallDE _ => false,
        CardType.RecogniseArticleDE x => x.n.Guts.Gender.IsPlural,
        CardType.RecallArticleDE x => x.n.Guts.Gender.IsPlural,
        _ => throw new ArgumentOutOfRangeException()
    };

    public object ConvertBack(object? value, Type targetType, object? parameter, CultureInfo culture)
    {
        throw new NotImplementedException();
    }
}
public class GenderConverter : IValueConverter
{
    public object Convert(object? value, Type targetType, object? parameter, CultureInfo culture) => (CardType)value! switch
    {
        CardType.RecogniseDE => false,
        CardType.RecallDE => false,
        CardType.RecogniseArticleDE => true,
        CardType.RecallArticleDE => true,
        _ => throw new ArgumentOutOfRangeException()
    };

    public object ConvertBack(object? value, Type targetType, object? parameter, CultureInfo culture)
    {
        throw new NotImplementedException();
    }
}
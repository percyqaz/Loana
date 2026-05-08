using Loana.Language;

namespace Loana.Mobile.Pages.Controls;

public class FlashcardView : ContentView
{
    public FlashcardView()
    { }

    public bool Revealed
    {
        get => (bool)GetValue(RevealedProperty);
        set => SetValue(RevealedProperty, value);
    }

    public static readonly BindableProperty RevealedProperty =
        BindableProperty.Create(
            propertyName: nameof(Revealed),
            returnType: typeof(bool),
            declaringType: typeof(FlashcardView),
            defaultValue: false,
            defaultBindingMode: BindingMode.OneWay,
            propertyChanged: OnRevealedChanged);

    private static void OnRevealedChanged(BindableObject bindable, object oldValue, object newValue)
    {
        ((FlashcardView)bindable).RedrawContent();
        Console.WriteLine("Hello");
    }

    public void RedrawContent()
    {
        var card = (Card)BindingContext;
        var layout = new VerticalStackLayout();

        var english = new HorizontalStackLayout()
        {
            Padding = new Thickness(15),
            BackgroundColor = Color.FromArgb("#FFFFFF"),
            VerticalOptions = LayoutOptions.Center
        };

        var deutsch = new HorizontalStackLayout()
        {
            Padding = new Thickness(15),
            BackgroundColor = GERMAN_BG,
            VerticalOptions = LayoutOptions.Center
        };

        var englishTop = card.Type switch
        {
            CardType.RecogniseDE => false,
            CardType.RecallDE => true,
            CardType.RecogniseArticleDE => false,
            CardType.RecallArticleDE => true,
            _ => throw new InvalidOperationException()
        };

        if (englishTop)
        {
            foreach (var e in EnglishSide(card)) { english.Children.Add(e); }
            if (Revealed)
            {
                foreach (var d in DeutschSide(card, true)) { deutsch.Children.Add(d); }
            }
            else
            {
                deutsch.Children.Add(
                    new Label()
                    {
                        Text = "???",
                        TextColor = Color.FromArgb("#FFFFFF"),
                        Padding = new Thickness(SPACE_SPACING, SPACE_SPACING)
                    });
            }
            layout.Children.Add(english);
            layout.Children.Add(deutsch);
        }
        else
        {
            foreach (var d in DeutschSide(card, Revealed)) { deutsch.Children.Add(d); }
            if (Revealed)
            {
                foreach (var e in EnglishSide(card)) { english.Children.Add(e); }
            }
            else
            {
                english.Children.Add(
                    new Label()
                    {
                        Text = "???",
                        TextColor = Color.FromArgb("#000000"),
                        Padding = new Thickness(SPACE_SPACING, SPACE_SPACING)
                    });
            }
            layout.Children.Add(deutsch);
            layout.Children.Add(english);
        }

        Content = layout;
    }

    protected override void OnBindingContextChanged()
    {
        RedrawContent();
    }

    private static readonly Color ENGLISH_NOTE = Color.FromArgb("#808080");
    private static readonly Color GERMAN_NOTE = Color.FromArgb("#C0C0C0");
    private static readonly Color GERMAN_BG = Color.FromArgb("#400000");
    private static readonly int SPACE_SPACING = 3;

    private static IEnumerable<IView> EnglishSide(Card card)
    {
        var definiteArticle = card.Type switch
        {
            CardType.RecogniseDE => false,
            CardType.RecallDE => false,
            CardType.RecogniseArticleDE => true,
            CardType.RecallArticleDE => true,
            _ => throw new InvalidOperationException()
        };
        if (definiteArticle)
            yield return new Label
            {
                Text = "the",
                TextColor = ENGLISH_NOTE,
                Padding = new Thickness(SPACE_SPACING, SPACE_SPACING)
            };
        var english = card.Type switch
        {
            CardType.RecogniseDE x => x.v.English,
            CardType.RecallDE x => x.v.English,
            CardType.RecogniseArticleDE x => x.n.English,
            CardType.RecallArticleDE x => x.n.English,
            _ => throw new InvalidOperationException()
        };
        var englishes = card.Type switch
        {
            CardType.RecogniseDE x => x.v.EnglishAlternatives,
            CardType.RecallDE x => x.v.EnglishAlternatives,
            CardType.RecogniseArticleDE x => x.n.EnglishAlternatives,
            CardType.RecallArticleDE x => x.n.EnglishAlternatives,
            _ => throw new InvalidOperationException()
        };
        foreach (var x in Annotation(english, englishes.Length == 0))
        {
            yield return x;
        }
        for (int i = 0; i < englishes.Length; i++)
        {
            foreach (var x in Annotation(englishes[i], i + 1 == englishes.Length))
            {
                yield return x;
            }
        }
    }

    private static IEnumerable<IView> Annotation(Annotation a, bool last)
    {
        if (Microsoft.FSharp.Core.FSharpOption<string>.get_IsSome(a.Note))
        {
            yield return new Label
            {
                Text = a.Text,
                TextColor = Color.FromArgb("#000000"),
                Padding = new Thickness(SPACE_SPACING, SPACE_SPACING)
            };
            yield return new Label
            {
                Text = $"[{a.Note.Value}]{(last ? "" : ",")}",
                TextColor = ENGLISH_NOTE,
                Padding = new Thickness(SPACE_SPACING, SPACE_SPACING)
            };
        }
        else if (!last)
        {
            yield return new Label
            {
                Text = a.Text,
                TextColor = Color.FromArgb("#000000"),
                Padding = new Thickness(SPACE_SPACING, SPACE_SPACING, 0, SPACE_SPACING)
            };
            yield return new Label
            {
                Text = ",",
                TextColor = ENGLISH_NOTE,
                Padding = new Thickness(0, SPACE_SPACING, SPACE_SPACING, SPACE_SPACING)
            };
        }
        else
        {
            yield return new Label
            {
                Text = a.Text,
                TextColor = Color.FromArgb("#000000"),
                Padding = new Thickness(SPACE_SPACING, SPACE_SPACING)
            };
        }
    }

    private static IEnumerable<View> DeutschSide(Card card, bool revealed)
    {
        var deutsch = card.Type switch
        {
            CardType.RecogniseDE x => x.v.Deutsch,
            CardType.RecallDE x => x.v.Deutsch,
            CardType.RecogniseArticleDE x => x.n.Deutsch,
            CardType.RecallArticleDE x => x.n.Deutsch,
            _ => throw new InvalidOperationException()
        };
        var gender = card.Type switch
        {
            CardType.RecogniseDE => null,
            CardType.RecallDE => null,
            CardType.RecogniseArticleDE x => x.n.Guts.Gender,
            CardType.RecallArticleDE x => x.n.Guts.Gender,
            _ => throw new InvalidOperationException()
        };
        if (gender?.IsMasculine == true)
        {
            yield return new Label
            {
                Text = "der",
                TextColor = GERMAN_NOTE,
                Padding = new Thickness(SPACE_SPACING, SPACE_SPACING)
            };
        }
        if (gender?.IsFeminine == true)
        {
            yield return new Label
            {
                Text = "die",
                TextColor = GERMAN_NOTE,
                Padding = new Thickness(SPACE_SPACING, SPACE_SPACING)
            };
        }
        if (gender?.IsNeuter == true)
        {
            yield return new Label
            {
                Text = "das",
                TextColor = GERMAN_NOTE,
                Padding = new Thickness(SPACE_SPACING, SPACE_SPACING)
            };
        }
        if (gender?.IsPlural == true)
        {
            yield return new Label
            {
                Text = "die",
                TextColor = GERMAN_NOTE,
                Padding = new Thickness(SPACE_SPACING, SPACE_SPACING)
            };
        }
        yield return new Label
        {
            Text = deutsch,
            TextColor = revealed && (gender is not null) ? Color.FromInt(gender!.Color.ToArgb()) : Color.FromArgb("#FFFFFF"),
            Padding = new Thickness(SPACE_SPACING, SPACE_SPACING)
        };
    }
}
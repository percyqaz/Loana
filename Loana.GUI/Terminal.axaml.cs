using Avalonia;
using Avalonia.Controls;
using Avalonia.Media;
using AvaloniaEdit.Document;
using AvaloniaEdit.Rendering;
using System;
using System.Collections.Generic;

namespace Loana.GUI;

public record StyledSpan(int Start, int Length, IBrush Foreground, IBrush Background);
public record ButtonSpan(int Start, int End, string Command);

public class DirectColorizer : DocumentColorizingTransformer
{
    // A list of styled ranges
    private readonly List<StyledSpan> _spans;

    public DirectColorizer(List<StyledSpan> spans)
    {
        _spans = spans;
    }

    protected override void ColorizeLine(DocumentLine line)
    {
        int lineStart = line.Offset;
        int lineEnd = line.EndOffset;

        foreach (var span in _spans)
        {
            int spanStart = span.Start;
            int spanEnd = span.Start + span.Length;

            if (spanEnd <= lineStart || spanStart >= lineEnd)
                continue;

            ChangeLinePart(
                Math.Max(spanStart, lineStart),
                Math.Min(spanEnd, lineEnd),
                visual =>
                {
                    visual.TextRunProperties.SetForegroundBrush(span.Foreground);
                    visual.TextRunProperties.SetBackgroundBrush(span.Background);
                });
        }
    }
}

public partial class Terminal : UserControl, IOutput
{
    private List<StyledSpan> styledSpans = [];
    private List<ButtonSpan> buttonSpans = [];

    public delegate void ButtonClickHandler(string command);

    public event ButtonClickHandler? OnButtonClicked;

    public Terminal()
    {
        InitializeComponent();
        TextEditor.TextArea.TextView.LineTransformers.Add(new DirectColorizer(styledSpans));
        TextEditor.TextArea.TextView.PointerWheelChanged += (_, e) => e.Handled = true;
        TextEditor.TextArea.PointerPressed += (_, e) =>
        {
            var position = TextEditor.GetPositionFromPoint(e.GetPosition(TextEditor));
            if (position is null)
            {
                return;
            }
            var offset = TextEditor.Document.GetOffset(position.Value.Line, position.Value.Column);
            var button = buttonSpans.Find(span => offset >= span.Start && offset < span.End);
            if (button != null)
            {
                OnButtonClicked?.Invoke(button.Command);
                e.Handled = true;
            }
        };
    }

    public void Write(string text, IBrush? foreground, IBrush? background)
    {
        var offset = TextEditor.Document.TextLength;
        TextEditor.Document.Insert(offset, text);
        var span = new StyledSpan(offset, text.Length, foreground ?? Brushes.LightGray, background ?? Brushes.Black);
        styledSpans.Add(span);
        TextEditor.TextArea.TextView.Redraw();
        TextEditor.TextArea.Caret.BringCaretToView(-10.0);
    }

    public void Button(string text, string command, IBrush? foreground, IBrush? background)
    {
        var offset = TextEditor.Document.TextLength;
        var button = new ButtonSpan(offset, offset + text.Length, command);
        buttonSpans.Add(button);
        Write(text, foreground, background);
    }

    public void Clear()
    {
        styledSpans.Clear();
        buttonSpans.Clear();
        TextEditor.Clear();
    }
}
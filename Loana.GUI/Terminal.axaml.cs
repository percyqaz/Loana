using Avalonia;
using Avalonia.Controls;
using Avalonia.Media;
using AvaloniaEdit.Document;
using AvaloniaEdit.Rendering;
using System;
using System.Collections.Generic;

namespace Loana.GUI;

public record StyledSpan(int Start, int Length, IBrush Foreground, IBrush Background);

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

public partial class Terminal : UserControl, Interface.IOutput
{
    private IBrush gradient = new LinearGradientBrush
    {
        StartPoint = new RelativePoint(0, 0, RelativeUnit.Relative),
        EndPoint = new RelativePoint(1, 0, RelativeUnit.Relative),
        GradientStops = new GradientStops
    {
        new GradientStop(Colors.Red, 0.0),   // start color at offset 0
        new GradientStop(Colors.Blue, 1.0)   // end color at offset 1
    }
    };

    private List<StyledSpan> styledSpans = [];

    public Terminal()
    {
        InitializeComponent();
        TextEditor.TextArea.TextView.LineTransformers.Add(new DirectColorizer(styledSpans));
        TextEditor.TextArea.TextView.PointerWheelChanged += (_, e) => e.Handled = true;
    }

    public void WriteLine(string text, IBrush? foreground)
    {
        Write(text + "\n", foreground);
    }

    public void Write(string text, IBrush? foreground)
    {
        var offset = TextEditor.Document.TextLength;
        TextEditor.Document.Insert(offset, text);
        var span = new StyledSpan(offset, text.Length, foreground ?? Brushes.LightGray, Brushes.Black);
        styledSpans.Add(span);
        TextEditor.TextArea.TextView.Redraw();
        TextEditor.TextArea.Caret.BringCaretToView(-10.0);
    }

    public void Clear()
    {
        styledSpans.Clear();
        TextEditor.Clear();
    }
}
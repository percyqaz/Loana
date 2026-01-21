namespace Loana.GUI

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open TheArtOfDev.HtmlRenderer.Avalonia

type App() =
    inherit Application()

    static let mutable main_window_gen = Unchecked.defaultof<_>

    override this.Initialize() =
        this.Styles.Add (FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- main_window_gen()
        | _ -> ()

    static member Run(constructor: unit -> HostWindow) =
        main_window_gen <- constructor
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime([||])

type HtmlWindow() =
    inherit HostWindow()

    let mutable _wv = None
    let mutable initial_html = ""
    let mutable initial_css = ""

    do
        base.Title <- "Loana"
        base.Width <- 400
        base.Height <- 500
        base.Content <-
            Component(fun ctx ->
                ViewBuilder.Create<HtmlPanel>(
                    attrs = [
                        HtmlPanel.horizontalAlignment HorizontalAlignment.Stretch
                        HtmlPanel.verticalAlignment VerticalAlignment.Stretch
                        HtmlPanel.onInitialized (fun (wv: HtmlPanel) ->
                            _wv <- Some wv
                            wv.Text <- initial_html
                            wv.BaseStylesheet <- initial_css
                        )
                    ]
                )
            )

    member this.SetHtml(html: string) =
        match _wv with
        | None -> initial_html <- html
        | Some wv -> wv.Text <- html

    member this.SetCSS(css: string) =
        match _wv with
        | None -> initial_css <- css
        | Some wv -> wv.BaseStylesheet <- css

    static member GetResource(key: string) : string =
        use stream = System.Reflection.Assembly.GetAssembly(typeof<HtmlWindow>).GetManifestResourceStream(typeof<HtmlWindow>, key)
        use sr = new System.IO.StreamReader(stream)
        sr.ReadToEnd()
namespace Loana.GUI

open System.Threading
open System.Threading.Tasks
open Avalonia
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Threading
open TheArtOfDev.HtmlRenderer.Avalonia

type App() =
    inherit Application()

    static let mutable running = false

    override this.Initialize() =
        this.Styles.Add (FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

    static member StartThread() =
        if running then () else

        running <- true
        let thread =
            Thread(ThreadStart(fun () ->
                AppBuilder
                    .Configure<App>()
                    .UsePlatformDetect()
                    .StartWithClassicDesktopLifetime([||], fun (lifetime: Controls.ApplicationLifetimes.IClassicDesktopStyleApplicationLifetime) -> lifetime.ShutdownMode <- Controls.ShutdownMode.OnExplicitShutdown)
                |> ignore
            ))

        thread.IsBackground <- true
        thread.SetApartmentState(ApartmentState.STA)
        thread.Start()

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

    static member ShowUntilClosed(init: HtmlWindow -> HtmlWindow) =

        let tcs = TaskCompletionSource()

        Dispatcher.UIThread.Post(fun () ->
            let w = init(HtmlWindow())
            w.Closed.Add(fun _ -> tcs.SetResult())
            w.Show()
        )

        tcs.Task.Wait()
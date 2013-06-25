namespace global

open System.Drawing
open System.IO
open System.Windows
open System.Windows.Forms.Integration
open FSharp.Charting
open FSharp.Charting.ChartTypes

type ChartWindow(chart:GenericChart, ?title) as this =

    inherit Window()

    let chartHost = new WindowsFormsHost()

    let mutable chart = chart |> Chart.WithXAxis(MajorGrid = Grid(LineColor = Color.LightGray))
                              |> Chart.WithYAxis(MajorGrid = Grid(LineColor = Color.LightGray))
                              |> Chart.WithMargin(5.0, 5.0, 5.0, 5.0)

    let setChart() =
        let chartControl = new ChartControl(chart)
        chartHost.Child <- chartControl
        chartControl
    
    let mutable chartControl = setChart()

    do
        this.Width <- 600.0
        this.Height <- 600.0
        this.Content <- chartHost
        //TODO broken with new FSharp.Charting
        //this.Title <- defaultArg title (ChartFormUtilities.ProvideTitle chart)
        this.Title <- defaultArg title "Untitled"
        setChart |> ignore

    member x.Reset(newChart) =
        chart <- newChart
        chartControl.Dispose()
        chartControl <- setChart()
        x.Activate() |> ignore

    //TODO broken with new FSharp.Charting: combining looses axis setup
    member x.Combine(newChart) =
        chart <- Chart.Combine [chart; newChart]
        chartControl.Dispose()
        chartControl <- setChart()
        x.Activate() |> ignore

    static member showWithTitle title chart =
        let chartWindow = new ChartWindow(chart, title)
        chartWindow.Show()
        chartWindow.Activate() |> ignore
        chartWindow

    static member show chart =
        let chartWindow = new ChartWindow(chart)
        chartWindow.Show()
        chartWindow.Activate() |> ignore
        chartWindow

    static member runWithTitle title chart = 
        let chartWindow = new ChartWindow(chart, title)
        chartWindow.ShowDialog() |> ignore

    static member run chart = 
        let chartWindow = new ChartWindow(chart)
        chartWindow.ShowDialog() |> ignore
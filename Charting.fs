module Charting

open System.Drawing
open FSharp.Charting
open FSharp.Charting.ChartTypes

let simplifyLine maxPoints points =
    let n = Array.length points
    if points.Length <= maxPoints then
        points
    else
        seq {
            for i in 0 .. (max 1 (n / maxPoints)) .. n-1 do
                yield points.[i]
        } |> Array.ofSeq

let plotIterations title functionTitle values =
    values
    |> Seq.mapi (fun i x -> (i, x))
    |> Array.ofSeq
    |> simplifyLine 200
    |> Chart.Line
    |> Chart.WithXAxis(Min = 0.0, Title = "Number of iterations")
    |> Chart.WithYAxis(Title = functionTitle)
    |> ChartWindow.showWithTitle title

let withRedCrossMarkerStyle c = c |> Chart.WithMarkers(Color.Red, 10, Style = MarkerStyle.Cross)

let linspace min max (n:int) =
    [| min .. (max - min) / (float n) .. max |]

open System.Windows
open Microsoft.Research.DynamicDataDisplay
open Microsoft.Research.DynamicDataDisplay.Common.Auxiliary
open Microsoft.Research.DynamicDataDisplay.Charts
open Microsoft.Research.DynamicDataDisplay.Charts.Navigation
open Microsoft.Research.DynamicDataDisplay.DataSources.MultiDimensional

let plotContour title (xLabel, yLabel) (xMin, yMin) (xMax, yMax) f = 

    let xValues = linspace -10.0 10.0 100
    let yValues = linspace -1.0 4.0 100
    let points = Array2D.init 101 101 (fun i j -> new Point(xValues.[i], yValues.[j]))
    let fValues = Array2D.init 101 101 (fun i j -> f (xValues.[i], yValues.[j]))
    let dataSource = new WarpedDataSource2D<_>(fValues, points)

    let plotter = new ChartPlotter()
    plotter.Children.Add(new HorizontalAxisTitle(Content = xLabel))
    plotter.Children.Add(new VerticalAxisTitle(Content = yLabel))

    let isolineGraph = new IsolineGraph()
    plotter.Children.Add(isolineGraph)    

    let trackingGraph = new IsolineTrackingGraph()
    plotter.Children.Add(trackingGraph)

    plotter.Children.Add(new CursorCoordinateGraph())

    isolineGraph.DataSource <- dataSource
    trackingGraph.DataSource <- dataSource

    let visible = dataSource.GetGridBounds()
    plotter.Viewport.Visible <- visible

    let window = new Window(Width = 600.0, Height = 600.0, Content = plotter, Title = title)
    window.Show() |> ignore

open System

// from http://www.codeproject.com/KB/WPF/WPFChart3D.aspx
let plotSurface title (xMin, yMin) (xMax, yMax) (f: float * float -> float) =
    let f = new Func<_,_,_>(fun x y -> f (x, y))
    let window = new WPFChart3D.SurfacePlotWindow(xMin, xMax, yMin, yMax, f, Title = title)
    window.Show() |> ignore

// from http://www.codeproject.com/KB/graphics/surfacePloter.aspx
let plotSurface2 title (xMin, yMin) (xMax, yMax) (f: float * float -> float) =
    //let f (x1, x2) = sin(x1)*cos(x2)/(sqrt(sqrt(x1*x1+x2*x2))+1.0)*10.0
    let f = new Func<_,_,_>(fun x y -> f (x, y))
    SurfacePlotWindow.show title f (5.0, 10.0, 200.0)

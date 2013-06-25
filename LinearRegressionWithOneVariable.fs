module LinearRegressionWithOneVariable

open System.IO
open FSharp.Charting
                           
let h (θ0, θ1) x = θ0 + θ1 * x

let J data θ =
    let m = List.length data |> float
    1. / (2. * m) * List.sumBy (fun (x, y) -> (h θ x - y) ** 2.) data
    
let private innerGradientDescent iterationFunction α maxIterations data =
    let m = List.length data |> float    
    
    let iteration θ = 
        (fst θ - (α / m) * List.sumBy (fun (x, y) -> h θ x - y) data,
         snd θ - (α / m) * List.sumBy (fun (x, y) -> (h θ x - y) * x) data)

    (0.0, 0.0) |> iterationFunction iteration maxIterations

let gradientDescent = innerGradientDescent Iteration.iterateUntilConvergence
let gradientDescentWithIntermediateResults = innerGradientDescent Iteration.iterateUntilConvergenceWithIntermediateResults

let plotGradientDescentIterations α maxIterations data =
    
    let θiterations = 
        gradientDescentWithIntermediateResults α maxIterations data
        |> Array.ofSeq

    θiterations |> Seq.map (J data)
                |> Charting.plotIterations "Gradient Descent Iterations" "Cost J"
                |> ignore

    θiterations.[θiterations.Length - 1]  
                                 
let plot θ (data : (float*float) list) =
    Chart.Combine
        [ Chart.Point(data, Name = "Training Data") |> Charting.withRedCrossMarkerStyle
          Chart.Line([ for (x, _) in data -> (x, h θ x) ], Name = "Linear Regression") ]
    |> Chart.WithLegend()
    |> ChartWindow.showWithTitle "Linear Regression"

let loadData file =
    let loadLine (line: string) =
        let parts = line.Split(',') 
        assert (parts.Length = 2)
        (float parts.[0], float parts.[1])
    File.ReadLines(__SOURCE_DIRECTORY__ + "/data/" + file) 
    |> Seq.map loadLine
    |> List.ofSeq

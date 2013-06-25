#load "Setup.fsx"

let prepare data = 
    List.map (fun (x, y) -> (float x, float y)) data

let data = [(3, 2)
            (1, 2)
            (0, 1)
            (4, 3)] |> prepare

let θ = data |> LinearRegressionWithOneVariable.gradientDescent 0.1 1000
let J = θ |> LinearRegressionWithOneVariable.J data
let h = 2.0 |> LinearRegressionWithOneVariable.h θ
θ |> printfn "θ = %A"
J |> printfn "J(θ) = %A"
h |> printfn "h(2) = %A"

LinearRegressionWithOneVariable.plot θ data

LinearRegressionWithOneVariable.plotGradientDescentIterations 0.1 1000 data
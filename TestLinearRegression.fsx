#load "Setup.fsx"

open MathNet.Numerics.LinearAlgebra

let prepare data = 
    data |> List.map (fun (x, _) -> List.map (fun x -> float x) x) |> matrix,
    data |> List.map (fun (_, y) -> float y) |> vector

let X, y = [[2; 5; 1; 5], 460
            [1; 3; 2; 4], 23
            [1; 3; 2; 3], 315
            [8; 2; 1; 3], 178] |> prepare

let XNorm, μ, σ = LinearRegression.featureNormalize X
let θ = (XNorm, y) |> LinearRegression.gradientDescent 0.1 10000
let J = θ |> LinearRegression.J (XNorm, y)
let h = (vector [2.0; 3.0; 1.0; 4.0] - μ) ./ σ |> LinearRegression.h θ
θ |> printfn "θ = %A"
J |> printfn "J(θ) = %A"
h |> printfn "h(2) = %A"

LinearRegression.plotGradientDescentIterations 0.1 10000 (XNorm, y)

let θ2 = (X, y) |> LinearRegression.normalEquation
let J2 = θ2 |> LinearRegression.J (X, y)
let h2 = vector [2.0; 3.0; 1.0; 4.0] |> LinearRegression.h θ2
θ2 |> printfn "θ = %A"
J2 |> printfn "J(θ) = %A"
h2 |> printfn "h(2) = %A"

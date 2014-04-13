#load "Setup.fsx"

open MathNet.Numerics.LinearAlgebra
open FSharp.Charting

// ================ Part 1: Feature Normalization ================

let X, y = LinearRegression.loadData "ex1data2.txt"

printfn "First 10 examples from the dataset:"
for i = 0 to 10 do
    printfn " x = %O y = %O" (X.Row(i)) y.[i]

printfn "Normalizing Features ..."
let XNorm, μ, σ = LinearRegression.featureNormalize X

// ================ Part 2: Gradient Descent ================

printfn "Running gradient descent ..."

let α = 0.3
let num_iters = 100

let θ = (XNorm, y) |> LinearRegression.plotGradientDescentIterations α num_iters
printfn "θ computed from gradient descent: \n %O" θ 

let price = (vector [1650.0; 3.0] - μ) ./ σ |> LinearRegression.h θ
printfn "Predicted price of a 1650 sq-ft, 3 br house (using gradient descent):\n %A\n" price

// ================ Part 3: Normal Equations ================

printfn "Solving with normal equations..."

let θwithNormalEq = (X, y) |> LinearRegression.normalEquation
printfn "θ computed from the normal equations: \n %O" θwithNormalEq

let priceWithNormalEq = vector [1650.0; 3.0] |> LinearRegression.h θwithNormalEq
printfn "Predicted price of a 1650 sq-ft, 3 br house (using normal equations):\n %A\n" price

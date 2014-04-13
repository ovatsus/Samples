module LogisticRegression

open MathNet.Numerics.LinearAlgebra

let sigmoid x =
    1. / (1. + exp -x)

let h (θ: Vector<float>) x = 
    let n = Vector.length x
    assert (Vector.length θ = n + 1)
    let x = x |> Vector.insert 0 1.
    sigmoid (θ * x)

let J (X, y) (θ: Vector<float>) =
    assert (Matrix.rowCount X = Vector.length y)

    let X = X.InsertColumn(0, DenseVector.create (Matrix.rowCount X) 1.)
    let m = Matrix.rowCount X |> float

    let alternative1() =
        
        let hθ x = 
            sigmoid (θ * x)
        
        let cost i x =
            y.[i] * log (hθ x) + (1. - y.[i]) * log (1. - hθ x)
        
        1. / m * (X |> Matrix.Σrows cost)

    let alternative2() =
        let h = θ * X |> Vector.map sigmoid        
        let errors = -y .* (h |> Vector.map log) - (1. .- y) .* ((1. .- h) |> Vector.map log)
        1. / m * (errors * errors)
        
    alternative2()

let innerGradientDescent iterationFunction α maxIterations (X, y) =    
    assert (Matrix.rowCount X = Vector.length y)
        
    let X = X.InsertColumn(0, DenseVector.create (Matrix.rowCount X) 1.)
    let m = Matrix.rowCount X |> float

    let iteration1 θ =
        let h θ x = 
            sigmoid (θ * x)
        θ - (α / m) * (X |> Matrix.Σrows (fun i x -> ((h θ x) - y.[i]) * x))

    let iteration2 θ =
        let h = X * θ |> Vector.map sigmoid
        θ - (α / m) * X.Transpose() * (h - y)
    
    DenseVector.create (Matrix.columnCount X) 0. |> iterationFunction iteration2 maxIterations

let gradientDescent α = innerGradientDescent Iteration.iterateUntilConvergence α
let gradientDescentWithIntermediateResults α = innerGradientDescent Iteration.iterateUntilConvergenceWithIntermediateResults α

module LogisticRegressionWithRegularization

open MathNet.Numerics.LinearAlgebra

let sigmoid x =
    1. / (1. + exp -x)

let h (θ: Vector<float>) x = 
    let n = Vector.length x
    assert (Vector.length θ = n + 1)
    let x = x |> Vector.insert 0 1.
    sigmoid (θ * x)

let J (X, y) λ (θ: Vector<float>) =
    assert (Matrix.rowCount X = Vector.length y)

    let X = X.InsertColumn(0, DenseVector.create (Matrix.rowCount X) 1.)
    let m = Matrix.rowCount X |> float

    let θforReg = θ.Clone()
    θforReg.[0] <- 0.

    let h = θ * X |> Vector.map sigmoid        
    let errors = -y .* (h |> Vector.map log) - (1. .- y) .* ((1. .- h) |> Vector.map log)
    1. / m * (errors * errors) + λ / (2. * m) * (θforReg * θforReg)

let innerGradientDescent iterationFunction α maxIterations (λ: float) (X, y) =    
    assert (Matrix.rowCount X = Vector.length y)
        
    let X = X.InsertColumn(0, DenseVector.create (Matrix.rowCount X) 1.)
    let m = Matrix.rowCount X |> float

    let iteration (θ: Vector<float>) =        

        let θforReg = θ.Clone()
        θforReg.[0] <- 0.

        let h = θ * X |> Vector.map sigmoid                
        let errors = X.Transpose() * (h - y)
        θ - (α / m) * errors - λ / m * θforReg

    DenseVector.create (Matrix.columnCount X) 0. |> iterationFunction iteration maxIterations

let gradientDescent α = innerGradientDescent Iteration.iterateUntilConvergence α
let gradientDescentWithIntermediateResults α = innerGradientDescent Iteration.iterateUntilConvergenceWithIntermediateResults α

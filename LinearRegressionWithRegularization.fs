module LinearRegressionWithRegularization

open MathNet.Numerics.LinearAlgebra

let J (X, y) λ (θ: Vector<float>) =
    assert (Matrix.rowCount X = Vector.length y)

    let X = X.InsertColumn(0, DenseVector.create (Matrix.rowCount X) 1.)
    let m = Matrix.rowCount X |> float

    let θforReg = θ.Clone()
    θforReg.[0] <- 0.

    let errors = X * θ - y
    1. / (2. * m) * ((errors * errors) + λ * (θforReg * θforReg))    

let innerGradientDescent iterationFunction α maxIterations (λ: float) (X, y) =    
    assert (Matrix.rowCount X = Vector.length y)
        
    let X = X.InsertColumn(0, DenseVector.create (Matrix.rowCount X) 1.)
    let m = Matrix.rowCount X |> float

    let iteration (θ: Vector<float>) =

        let θforReg = θ.Clone()
        θforReg.[0] <- 0.

        let errors = X.Transpose() * (X * θ - y)
        θ - (α / m) * errors - λ / m * θforReg

    DenseVector.create (Matrix.columnCount X) 0. |> iterationFunction iteration maxIterations

let gradientDescent α = innerGradientDescent Iteration.iterateUntilConvergence α
let gradientDescentWithIntermediateResults α = innerGradientDescent Iteration.iterateUntilConvergenceWithIntermediateResults α

let normalEquation λ (X, y) =    
    assert (Matrix.rowCount X = Vector.length y)
        
    let X = X.InsertColumn(0, DenseVector.create (Matrix.rowCount X) 1.)   
    let X' = X.Transpose()
    
    Matrix.inverse (X' * X + λ * DenseMatrix.identity<float> (X.ColumnCount)) * X' * y

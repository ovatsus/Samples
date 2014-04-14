module LinearRegression

open System.IO
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics

let h (θ: Vector<float>) x = 
    let n = Vector.length x
    assert (Vector.length θ = n + 1)
    let x = x |> Vector.insert 0 1.
    θ * x

let J (X, y) θ =
    assert (Matrix.rowCount X = Vector.length y)

    let X = X.InsertColumn(0, DenseVector.create (Matrix.rowCount X) 1.)
    let m = Matrix.rowCount X |> float

    let alternative1() = 
        1. / (2. * m) * Vector.Σ ((X * θ - y) .^ 2.)

    let alternative2() = 
        let errors = X * θ - y
        1. / (2. * m) * (errors * errors)
    
    alternative2()    

let innerGradientDescent iterationFunction α maxIterations (X, y) =    
    assert (Matrix.rowCount X = Vector.length y)
        
    let X = X.InsertColumn(0, DenseVector.create (Matrix.rowCount X) 1.)
    let m = Matrix.rowCount X |> float

    let iteration θ =
        θ - (α / m) * X.Transpose() * (X * θ - y)
    
    DenseVector.create (Matrix.columnCount X) 0. |> iterationFunction iteration maxIterations

let gradientDescent α = innerGradientDescent Iteration.iterateUntilConvergence α
let gradientDescentWithIntermediateResults α = innerGradientDescent Iteration.iterateUntilConvergenceWithIntermediateResults α

let plotGradientDescentIterations α maxIterations (X, y) =

    let θiterations = 
        gradientDescentWithIntermediateResults α maxIterations (X, y)
        |> Array.ofSeq

    θiterations |> Seq.map (J (X, y))
                |> Charting.plotIterations "Gradient Descent Iterations" "Cost J"
                |> ignore

    θiterations.[θiterations.Length - 1]        

let featureNormalize (X: Matrix<float>) =
    
    let μ = 
        X.EnumerateColumns()
        |> Seq.map (fun col -> col.Mean()) 
        |> DenseVector.ofSeq
    
    let σ = 
        X.EnumerateColumns() 
        |> Seq.map (fun col -> col.StandardDeviation()) 
        |> DenseVector.ofSeq    
    
    let alternative1() =
        let X = X |> Matrix.mapRows (fun i row -> (row - μ) ./ σ)
        (X, μ, σ)

    let alternative2() =
        let μExpanded = DenseMatrix.initRows X.RowCount (fun _ -> μ)
        let σDiag = DenseMatrix.ofDiag σ
        let X = (X - μExpanded) * σDiag.Inverse()
        (X, μ, σ)

    alternative2()

let normalEquation (X, y) =
    assert (Matrix.rowCount X = Vector.length y)
    
    let X = X.InsertColumn(0, DenseVector.create (Matrix.rowCount X) 1.)
    let X' = X.Transpose()

    (X' * X).Inverse() * X' * y

let loadData file =
    let matrix =
        File.ReadLines(__SOURCE_DIRECTORY__ + "/data/" + file) 
        |> Seq.map (fun line -> line.Split(',') |> Array.map float) 
        |> DenseMatrix.ofRowSeq
    let m = matrix.RowCount
    let n = matrix.ColumnCount
    (matrix.[0..,..n-2], matrix.Column(n-1))

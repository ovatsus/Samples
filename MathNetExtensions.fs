namespace MathNet.Numerics.LinearAlgebra

open MathNet.Numerics.LinearAlgebra

[<AutoOpen>]
module VectorOperators = 

    let inline (.^) (v: #Vector<float>) exp = 
        Vector.map (fun x -> x ** exp) v

    let inline (./) (v: #Vector<float>) divider = 
        v.PointwiseDivide(divider)

    let inline (.*) (v: #Vector<float>) divider = 
        v.PointwiseMultiply(divider)   

    let inline (.-) (scalar: float) (v: #Vector<float>) = 
        DenseVector.init v.Count (fun i -> scalar - v.[i])

module Vector = 
    let inline length (v: #Vector<float>) = v.Count
    let inline Σ (v: #Vector<float>) = v.Sum()

module Matrix =
    let inline rowCount (m: #Matrix<float>) = m.RowCount
    let inline columnCount (m: #Matrix<float>) = m.ColumnCount
    
    let inline Σrows f A = Matrix.sumRowsBy f A
    let inline Σcols f A = Matrix.sumColsBy f A
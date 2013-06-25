module Iteration

let iterateUntilConvergence iteration maxIterations x0 =
    let rec loop maxIterations x = 
        let x' = iteration x
        if x <> x' && maxIterations > 0 then
            loop (maxIterations-1) x'
        else
            x'
    loop maxIterations x0

let iterateUntilConvergenceWithIntermediateResults iteration maxIterations x0 =
    let rec loop maxIterations x = seq {
        yield x
        let x' = iteration x
        if x <> x' && maxIterations > 0 then
            yield! loop (maxIterations-1) x'
    }
    loop maxIterations x0

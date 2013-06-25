#load "Setup.fsx"

open FSharp.Charting

type Parameters = 
    { contactRate : float
      trasmissionRate : float
      populationSize : float }

let nextPeriodInfectedPeople p infectedPeople =
    let W = infectedPeople
    let N = p.populationSize
    let c = p.contactRate
    let t = p.trasmissionRate    
    W + N * c * t * (W / N) * (N - W)/N

let plot p periods initialInfectedPeople =
    (float initialInfectedPeople)
    |> Seq.unfold (fun infectedPeople -> Some(infectedPeople, nextPeriodInfectedPeople p infectedPeople)) 
    |> Seq.zip (Seq.initInfinite id)
    |> Seq.take periods
    |> Seq.toArray
    |> Chart.Line 

let p = { contactRate = 0.1; trasmissionRate = 0.1; populationSize = 100.0 }

plot p 1000 1 |> ChartWindow.show
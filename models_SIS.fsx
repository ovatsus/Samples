#load "Setup.fsx"

open FSharp.Charting

// SIS = Susceptible Infected Susceptible

type Parameters = 
    { contactRate : float
      trasmissionRate : float
      populationSize : float
      cureRate : float }

let nextPeriodInfectedPeople p infectedPeople =
    let W = infectedPeople
    let N = p.populationSize
    let c = p.contactRate
    let t = p.trasmissionRate    
    let a = p.cureRate
    W + W * (c * t * (N - W)/N - a)
    
// basic reproduction number
let r0 p = 
    let c = p.contactRate
    let t = p.trasmissionRate    
    let a = p.cureRate
    c * t / a

let goingToSpread p vaccinationPercentage =
    r0 p > 1.0  

let minimumVaccinationToNotSpread r0 =
    1.0 - 1.0 / r0

let plot p periods initialInfectedPeople =
    (float initialInfectedPeople)
    |> Seq.unfold (fun infectedPeople -> Some(infectedPeople, nextPeriodInfectedPeople p infectedPeople)) 
    |> Seq.take periods
    |> Chart.Line 

let p = { contactRate = 0.3; trasmissionRate = 0.3; populationSize = 100.0; cureRate = 0.1 }

plot p 1000 1
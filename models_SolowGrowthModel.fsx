#load "Setup.fsx"

open FSharp.Charting

type Parameters = 
    { labour : float
      investmentRate : float
      depreciationRate : float }

let gdp p machines = 
    p.labour * sqrt machines

let nextYearMachines p machines =
    machines + (gdp p machines) * p.investmentRate - machines * p.depreciationRate
    
let nextYearGdp p machines =
    gdp p (nextYearMachines p machines)
    
let equilibriumMachines p = 
    (p.investmentRate * p.labour / p.depreciationRate) ** 2.0

let growth p machines =
    nextYearGdp p machines / gdp p machines - 1.0

let equilibriumGdp p = 
    gdp p (equilibriumMachines p)

let plotGdp p initialMachines years =
    Seq.unfold (fun machines -> Some(gdp p machines, nextYearMachines p machines)) initialMachines
    |> Seq.zip (Seq.initInfinite id)
    |> Seq.take years
    |> Seq.toArray
    |> Chart.Line

let plotGrowth p initialMachines years =
    initialMachines
    |> Seq.unfold (fun machines -> Some(growth p machines, nextYearMachines p machines))
    |> Seq.map (fun growth -> growth * 100.0)
    |> Seq.zip (Seq.initInfinite id)
    |> Seq.take years
    |> Seq.toArray
    |> Chart.Column

let p = { labour = 100.0; investmentRate = 0.2; depreciationRate = 0.1 }
let machines = 3600.0

plotGdp p machines 50 |> ChartWindow.showWithTitle "China GDP"
plotGrowth p machines 50 |> ChartWindow.showWithTitle "China Growth"

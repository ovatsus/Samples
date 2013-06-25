let world = ["green"; "red"; "red"; "green"; "green"]
let measurements = ["red"; "green"]
let motions = [1;1]
let pHit = 0.6
let pMiss = 0.2
let pExact = 0.8
let pOvershoot = 0.1
let pUndershoot = 0.1

open System.Linq

let range n = { 0..n-1 }

let sense p Z =
    let p = p |> List.mapi (fun i item -> item * (if Z = world.[i] then pHit else pMiss))
    let s = List.sum p
    p |> List.map (fun item -> item / s)

let move (p : float list) U = 
    let getP i prob = 
        p.[(i - U + p.Length) % p.Length] * prob
    [for i in range(world.Length) -> getP i pExact + getP (i - 1) pOvershoot + getP (i + 1) pUndershoot]

let run() = 
    let mutable p = [for _ in range(world.Length) -> world.Length |> float]
    for i in range(measurements.Length) do
        p <- sense p measurements.[i]
        p <- move p motions.[i]
    printfn "%A" p

run()

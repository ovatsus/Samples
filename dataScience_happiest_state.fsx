#load "Setup.fsx"

open System
open System.IO
open System.Collections.Generic
open FSharp.Data
open FSharp.Data.Json.Extensions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let scores = Dictionary<_,_>()
for line in File.ReadLines "data/AFINN-111.txt" do
    let [|term; score|] = line.Split '\t'
    scores.[term] <- Int32.Parse score

let asOption = function 
| true, value -> Some value
| false, _ -> None

let states = Dictionary<_,_>()

for line in File.ReadLines "data/output.json" do
    let twitter = JsonProvider<"data/sample.json", SampleList=true>.Parse line    
    match twitter.Text, twitter.Place with
    | Some text, Some place ->
        if place.CountryCode = "US" then
            let state = place.FullName.Substring(place.FullName.Length-3)
            let score = 
                scores
                |> Seq.sumBy (fun (KeyValue(key,value)) ->
                    if text.Contains key then value else 0)
            let current = defaultArg (states.TryGetValue state |> asOption) 0
            states.[state] <- current + score
    | _ -> ()

for KeyValue(key, value) in states |> Seq.sortBy (fun (KeyValue(_, value)) -> -value) do
    printfn "%s %d" key value

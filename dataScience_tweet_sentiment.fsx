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

for line in File.ReadLines "data/output.json" do
    let twitter = JsonProvider<"data/sample.json", SampleList=true>.Parse line
    let score = 
        match twitter.Text with
        | Some text ->
            scores
            |> Seq.sumBy (fun (KeyValue(key,value)) ->
                if text.Contains key then value else 0)
        | None -> 0
    printfn "%d" score
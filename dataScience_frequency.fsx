#load "Setup.fsx"

open System
open System.IO
open System.Collections.Generic
open FSharp.Data
open FSharp.Data.Json.Extensions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let asOption = function 
| true, value -> Some value
| false, _ -> None

let termCounts = Dictionary<_,_>()
let mutable totalCount = 0

for line in File.ReadLines "data/output.json" do
    let twitter = JsonProvider<"data/sample.json", SampleList=true>.Parse line
    match twitter.Text with
    | None -> ()    
    | Some text ->
        let words = text.Split([|' '; '\t'; '\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
        words
        |> Seq.iter(fun word ->
            let currentCount = defaultArg (termCounts.TryGetValue word |> asOption) 0.
            termCounts.[word] <- currentCount + 1.
            totalCount <- totalCount + 1)

for key in termCounts.Keys |> Seq.toArray do
    termCounts.[key] <- termCounts.[key] / (float totalCount)
    let value = termCounts.[key] 
    printfn "%s %f" key value

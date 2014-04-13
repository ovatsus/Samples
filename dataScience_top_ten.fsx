#load "Setup.fsx"

open System
open System.IO
open System.Collections.Generic
open FSharp.Data
open FSharp.Data.JsonExtensions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let asOption = function 
| true, value -> Some value
| false, _ -> None

let hashTags = Dictionary<_,_>()

for line in File.ReadLines "data/output.json" do
    let twitter = JsonProvider<"data/sample.json", SampleIsList=true>.Parse line    
    match twitter.Entities with
    | None -> ()    
    | Some entities ->
        for hashTag in entities.Hashtags do
            let hashTag = hashTag.Text
            let currentCount = defaultArg (hashTags.TryGetValue hashTag |> asOption) 0
            hashTags.[hashTag] <- currentCount + 1

for KeyValue(key, value) in hashTags |> Seq.sortBy (fun (KeyValue(_, value)) -> -value) |> Seq.take 10 do
    printfn "%s %d" key value

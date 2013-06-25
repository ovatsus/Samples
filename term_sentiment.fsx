#r @"packages\FSharp.Data.1.1.5\lib\net40\FSharp.Data.dll"
open System
open System.IO
open System.Collections.Generic
open FSharp.Data
open FSharp.Data.Json.Extensions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let asOption = function 
| true, value -> Some value
| false, _ -> None

let scores = Dictionary<_,_>()
for line in File.ReadLines "AFINN-111.txt" do
    let [|term; score|] = line.Split '\t'
    scores.[term] <- Double.Parse score

let terms = Dictionary<_,_>()

for line in File.ReadLines "output.json" do
    let twitter = JsonProvider<"sample.json", SampleList=true>.Parse line
    match twitter.Text with
    | None -> ()    
    | Some text ->
        let score = 
            scores
            |> Seq.sumBy (fun (KeyValue(key,value)) ->
                if text.Contains key then value else 0.)
        let words = text.Split([|' '; '\t'; '\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
        words
        |> Seq.filter (not << scores.ContainsKey)
        |> Seq.iter(fun word ->
            let currentScore = defaultArg (terms.TryGetValue word |> asOption) 0.
            terms.[word] <- currentScore + (score / (float words.Length)))

for KeyValue(key,value) in terms do
    printfn "%s %f" key value
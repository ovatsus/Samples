#load "Setup.fsx"

open FSharp.Net
open FSharp.Data
open FSharp.Data.Json
open FSharp.Data.Json.Extensions

for page in 1..10 do
    let response = Http.Request <| "http://search.twitter.com/search.json?q=microsoft&page=" + (string page)
    let json = JsonValue.Parse response
    let results = json.["results"]

    for item in results do
        printfn "%O" item?text
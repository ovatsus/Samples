
let rec mergeSort arr =

    let split arr =
        let n = Array.length arr
        arr.[0..n/2-1], arr.[n/2..n-1]
 
    let merge l r =
        let n = Array.length l + Array.length r
        let res = Array.zeroCreate n
        let mutable i, j = 0, 0
        for k = 0 to n-1 do
            if i >= l.Length   then res.[k] <- r.[j]; j <- j + 1
            elif j >= r.Length then res.[k] <- l.[i]; i <- i + 1
            elif l.[i] < r.[j] then res.[k] <- l.[i]; i <- i + 1
            else                    res.[k] <- r.[j]; j <- j + 1 
        res

    match arr with
    | [||]  -> [||]
    | [|a|] -> [|a|]
    | arr   -> let x, y = split arr
               merge (mergeSort x) (mergeSort y)

 
let countInversions arr = 

    let countSplitInversions l r =
        let n = Array.length l + Array.length r
        let res = Array.zeroCreate n
        let mutable i, j, inv = 0, 0, 0L
        for k = 0 to n-1 do
            if i >= l.Length   then res.[k] <- r.[j]; j <- j + 1
            elif j >= r.Length then res.[k] <- l.[i]; i <- i + 1
            elif l.[i] < r.[j] then res.[k] <- l.[i]; i <- i + 1
            else                    res.[k] <- r.[j]; j <- j + 1; inv <- inv + l.LongLength - (int64 i)
        res, inv
 
    let rec sortAndCount arr =
        match Array.length arr with
        | 0 | 1 -> arr, 0L
        | n -> let l, lInv = sortAndCount arr.[0..n/2-1]
               let r, rInv = sortAndCount arr.[n/2..n-1]
               let sorted, sInv = countSplitInversions l r
               sorted, lInv + rInv + sInv

    sortAndCount arr |> snd

open System
open System.IO

let progQuestion1() =
    let numbers = 
        File.ReadLines (Path.Combine(__SOURCE_DIRECTORY__, "data/IntegerArray.txt")) 
        |> Seq.map Int32.Parse
        |> Seq.toArray
        
    countInversions numbers

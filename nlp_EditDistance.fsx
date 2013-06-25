type EditType = Deletion | Insertion | Substitution

type DistanceType = 
    { Deletion: char -> int
      Insertion: char -> int
      Substitution: char -> char -> int }

let minimumEditDistance = 
    { Deletion = fun _ -> 1
      Insertion = fun _ -> 1
      Substitution = fun x y -> if x = y then 0 else 1}

let levenshteinDistance = 
    { Deletion = fun _ -> 1
      Insertion = fun _ -> 1
      Substitution = fun x y -> if x = y then 0 else 2}

let getEditDistance distanceType (X:string) (Y:string) =
    let m = X.Length
    let n = Y.Length
    let d = Array2D.create (m + 1) (n + 1) 0
    for i in 1..m do
        d.[i, 0] <- d.[i-1, 0] + distanceType.Deletion X.[i-1]
    for j in 1..n do
        d.[0, j] <- d.[0, j-1] + distanceType.Insertion Y.[j-1]
    let ptr = Array2D.init (m + 1) (n + 1) (fun i j -> if j = 0 then Deletion elif i = 0 then Insertion else Substitution)
    for i in 1..m do
        for j in 1..n do
            let a, b = Seq.minBy fst [d.[i-1, j] + distanceType.Deletion X.[i-1], Deletion
                                      d.[i, j-1] + distanceType.Insertion Y.[j-1], Insertion
                                      d.[i-1, j-1] + distanceType.Substitution X.[i-1] Y.[j-1], Substitution]
            d.[i, j] <- a
            ptr.[i, j] <- b
    let alignment = 
        (m, n) 
        |> Seq.unfold (fun (i, j) -> 
            if i = 0 && j = 0 then
                None
            else
                match ptr.[i, j] with
                | Deletion -> Some((X.[i-1], '*'), (i-1, j))
                | Insertion -> Some(('*', Y.[j-1]), (i, j-1))
                | Substitution -> Some((X.[i-1], Y.[j-1]), (i-1, j-1)))
        |> Array.ofSeq
        |> Array.rev
    d.[m, n], alignment

let printAlignment alignment = 
    let toString (chars : char array) = new string(chars)
    alignment |> Array.map fst |> toString |> printfn "%s"
    alignment |> Array.map snd |> toString |> printfn "%s"

let distanceM, alignmentM = getEditDistance minimumEditDistance "intention" "execution"

printfn "Minimum Edit Distance: %d" distanceM
printAlignment alignmentM

let distanceL, alignmentL = getEditDistance levenshteinDistance "intention" "execution"

printfn "Levenshtein Distance: %d" distanceL
printAlignment alignmentL

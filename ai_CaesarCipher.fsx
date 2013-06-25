open System

let CaesarCipher rotationAmount input = 
    input 
    |> Seq.map (fun c -> if 'a' <= c && c <= 'z' then
                                let c = (int c) + rotationAmount
                                char (if c > (int 'z') then c - (int 'z') + (int 'a') - 1 else c)
                            else if 'A' <= c && c <= 'Z' then
                                let c = (int c) + rotationAmount
                                char (if c > (int 'Z') then c - (int 'Z') + (int 'A') - 1 else c)
                            else
                                c)
    |> Array.ofSeq
    |> (fun chars -> new String(chars))

open System.Text.RegularExpressions
open System.Linq

let splitInWords (text:string) =
    Regex.Matches(text.ToLowerInvariant(), "[a-z]+").Cast<Match>()
    |> Seq.map (fun m -> m.Value)

open System.IO

let corpus = 
    // big.txt from http://norvig.com/spell-correct.html
    // The file is a concatenation of several public domain books from Project Gutenberg and lists of 
    // most frequent words from Wiktionary and the British National Corpus.
    File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "data/big.txt"))
    |> splitInWords 
    |> Seq.countBy id
    |> Map.ofSeq

let getPercentageOfKnownWords text =
    let words = splitInWords text
    let validWords = 
        splitInWords text
        |> Seq.sumBy (fun w -> if corpus.ContainsKey(w) then 1 else 0)
    float validWords / float (words.Count())

let decipher input = 
    [1..25]
    |> Seq.map (fun rotationAmount -> 
                    let text = (CaesarCipher rotationAmount input)
                    (rotationAmount, text, getPercentageOfKnownWords text))
    |> Seq.sortBy (fun (_, _, percentage) -> -percentage)
    |> Seq.head

let input = "Esp qtcde nzyqpcpynp zy esp ezatn zq Lcetqtntlw Tyepwwtrpynp hld spwo le Olcexzfes Nzwwprp ty estd jplc."

let decypheredInput = decipher input

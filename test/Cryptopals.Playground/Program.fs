open System
open System.IO
open Cryptopals
open Cryptopals.Tests
open System.Text

let private possibleKeys = seq { (byte 0)..(byte 255) }

/// create a character frequency map from the sample text
let corpus =
    File.ReadAllText "./data/aliceinwonderland.txt"
    |> Comparison.buildCorpus

[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllText "./data/set01/6.txt"
        |> Base64.decode
        |> Seq.toArray
    let key =
        Comparison.findBestRepeatingXorKey
            <| corpus
            <| input
        |> Seq.toArray

    let decrypted =
        Ciphers.xorStreamWithRepeatingKey key input
        |> Seq.toArray
        |> Encoding.ASCII.GetString
    printfn "key: %s" (Encoding.ASCII.GetString key)
    printfn "decrypted:"
    printfn "%s" decrypted

    0 // return an integer exit code

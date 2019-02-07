open System
open System.IO
open Cryptopals
open Cryptopals.Tests
open System.Text
open Cryptopals.Tests

[<EntryPoint>]
let main argv =
    /// create a character frequency map from the sample text
    let corpus =
        File.ReadAllBytes "./data/aliceinwonderland.txt"
        |> Comparison.buildCorpus

    let input =
        File.ReadAllText "./data/set01/6.txt"
        |> Base64.decode
        |> Seq.toArray

    // let keyLengths = Comparison.findRepeatingXorKeyLengths input
    // printfn "key lengths:"
    // for x in keyLengths do
    //     printfn "%A" x

    // let possibleKeys =
    //     keyLengths
    //     |> Seq.map (fun (_, keyLength) ->
    //         Comparison.findRepeatingXorKey
    //             <| corpus
    //             <| input
    //             <| keyLength
    //     )
    // let _, bestKey =
    //     Comparison.compareRepeatingXorKeys
    //         <| corpus
    //         <| possibleKeys
    //         <| input
    let bestKey: byte[] = Comparison.findRepeatingXorKey corpus input 29

    printfn "best key length: %d" bestKey.Length
    for idx, ch in (Seq.mapi (fun idx ch -> (idx, ch)) bestKey) do
        printfn "%3d %2s %s"
            <| idx
            <| Encoding.ASCII.GetString([|ch|])
            <| TestHelpers.stringifyBits [ ch ]

    // printfn "best key: \"%s\"" (Encoding.ASCII.GetString bestKey)

    // let key =
    //     Comparison.findBestRepeatingXorKey
    //         <| corpus
    //         <| keyLength
    //         <| input
    //     |> Seq.toArray

    // let decrypted =
    //     Ciphers.xorStreamWithRepeatingKey key input
    //     |> Seq.toArray
    //     |> Encoding.ASCII.GetString
    // printfn "key: %s" (Encoding.ASCII.GetString key)
    // printfn "decrypted:"
    // printfn "%s" decrypted

    0 // return an integer exit code

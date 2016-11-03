open Encoding
open System.Text

[<EntryPoint>]
let main argv =
    let seqToString = Seq.map string >> String.concat ""
    let stringifyBits (bytes:byte seq) =
        let getBit b = if b > byte 0 then '1' else '0'
        let getBits b =
            [0..7]
            |> Seq.map (fun i -> (b >>> i) &&& 0b1uy)
            |> Seq.map getBit
            |> Seq.rev
        bytes
            |> Seq.map (getBits >> seqToString)
            |> String.concat " "

    let each arg =
        // printfn "clear: %s" arg
        let bytes = Hex.decode arg
        printfn "bytes: %s" <| stringifyBits bytes
        // let hexEncoded = Hex.encode bytes |> seqToString
        // printfn "hex:   %s" hexEncoded

        let base64Encoded = Base64.encode bytes |> seqToString
        printfn "base64: %s" base64Encoded
        let base64Decoded = Base64.decode base64Encoded
        printfn "bytes:  %s" <| stringifyBits base64Decoded

        let hexRecoded = Hex.encode base64Decoded |> seqToString
        printfn "hex: %s" <| hexRecoded

    argv |> Seq.iter each

    0

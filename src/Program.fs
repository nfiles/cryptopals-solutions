open Encoding
open System
open System.Text

let hexToBase64 = Hex.decode >> Base64.encode
let base64ToHex = Base64.decode >> Hex.encode

let stringCompare (comparison: StringComparison) s1 s2 =
    String.Compare(s1, s2, comparison)

[<EntryPoint>]
let main argv =
    let seqToString = Seq.map string >> String.concat ""
    let stringifyBits (bytes:byte seq) =
        let pickBitFromByte b i = (b >>> i) &&& 0b1uy
        let bitToChar b = if b > byte 0 then '1' else '0'
        let getBits b =
            [0..7]
            |> Seq.map (pickBitFromByte b >> bitToChar)
            |> Seq.rev

        bytes
            |> Seq.map (getBits >> seqToString)
            |> String.concat " "

    let stringsAreEqual s1 s2 =
        stringCompare StringComparison.OrdinalIgnoreCase s1 s2
            |> (=) 0

    let each hexInput =
        let base64 = hexToBase64 hexInput
        let base64String = base64 |> seqToString
        printfn "base64: %s" base64String

        let hexRecoded = base64ToHex base64
        let hexRecodedString = hexRecoded |> seqToString
        printfn "hex:    %s" hexRecodedString

        printfn "Do the strings match? %s"
            <| if stringsAreEqual hexInput hexRecodedString then "Yes!" else "No..."

    argv |> Seq.iter each

    0

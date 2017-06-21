namespace Cryptopals.Tests

module TestHelpers =
    open System

    let stringifyBits (bytes:byte seq) =
        let getBits b =
            let bitToChar b = if b > byte 0 then '1' else '0'
            let getBit i = (b >>> i) &&& 0b1uy
            [0..7] |> Seq.rev |> Seq.map (getBit >> bitToChar)
        bytes
            |> Seq.map (getBits >> String.Concat)
            |> String.concat " "

    let stringCompare (comparison: StringComparison) s1 s2 =
        String.Compare(s1, s2, comparison)

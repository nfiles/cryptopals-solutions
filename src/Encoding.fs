(*
    base 64 decoding
    1. encode/decode
*)

namespace Encoding

module Test =
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


module Hex =
    open System

    let decode chars =
        let decodeChar c =
            match Char.ToUpper c with
                | '0' -> 0
                | '1' -> 1
                | '2' -> 2
                | '3' -> 3
                | '4' -> 4
                | '5' -> 5
                | '6' -> 6
                | '7' -> 7
                | '8' -> 8
                | '9' -> 9
                | 'A' -> 10
                | 'B' -> 11
                | 'C' -> 12
                | 'D' -> 13
                | 'E' -> 14
                | 'F' -> 15
                | _ -> failwith "Invalid hex character"
                |> byte

        let decodeByte chars =
            match List.ofSeq chars with
                | a::b::_ -> (decodeChar a <<< 4) + decodeChar b
                | a::_ -> decodeChar a
                | _ -> 0b00000000uy

        chars |> (Seq.chunkBySize 2 >> Seq.map decodeByte)

    let encode bytes =
        let encodeChar n =
            if n > byte 16 then
                failwith "Invalid nibble! Must be less than 16"
            "0123456789ABCDEF".[int n]

        let encodeByte byte =
            [| byte >>> 4; byte |]
                |> Seq.map ((&&&) 0b1111uy >> encodeChar)

        bytes |> Seq.collect encodeByte

module Base64 =
    let HASH = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    let LOOKUP =
        HASH
            |> Seq.mapi (fun i c -> (c, byte i))
            |> dict

    let private padSequenceRight pad length source =
        [source; Seq.initInfinite (fun i -> pad)]
            |> Seq.concat
            |> Seq.take length

    let encode bytes =
        let encodeChunk bytes =
            let padChunk = padSequenceRight 0b0uy 3 >> Seq.toArray

            match padChunk bytes with
                | [|a;b;c|] ->
                    // printfn "sextet chunks: %s" <| Test.stringifyBits [
                    //     a >>> 2
                    //     a &&& 0b11uy <<< 4
                    //     b >>> 4
                    //     b &&& 0b1111uy <<< 2
                    //     c >>> 6
                    //     c &&& 0b111111uy
                    // ]
                    [|
                        a >>> 2
                        ((a &&& 0b11uy) <<< 4) + (b >>> 4)
                        ((b &&& 0b1111uy) <<< 2) + (c >>> 6)
                        c &&& 0b111111uy
                    |] //|> (fun x -> printfn "sextets: %s" <| Test.stringifyBits x; x)
                | _ -> failwith "Must have between one and three bytes"
                |> Seq.take (Array.length bytes + 1)
                |> Seq.map (fun i -> HASH.[int i])

        bytes
            |> Seq.chunkBySize 3
            |> Seq.collect encodeChunk

    let decode (chars:char seq) =
        let getByteOrDefault c = if LOOKUP.ContainsKey c then LOOKUP.[c] else 0b0uy
        let padChunk =
            padSequenceRight '=' 4
            >> Seq.map getByteOrDefault
            >> Seq.toArray

        let decodeChunk chars =
            let length = if Array.length chars > 3 then 3 else Array.length chars
            // printfn "chunks: %A" <| chars
            // printfn "padded chunk: %A" <| padChunk chars
            match padChunk chars with
                | [|a;b;c;d|] ->
                    // printfn "sextets: %s" <| Test.stringifyBits [a;b;c;d]
                    // printfn "byte pieces: %s" <| Test.stringifyBits [
                    //     a <<< 2
                    //     (b &&& 0b110000uy) >>> 4
                    //     b <<< 4
                    //     c >>> 2
                    //     c <<< 6
                    //     d
                    // ]
                    [|
                        (a <<< 2) + ((b &&& 0b110000uy) >>> 4)
                        (b <<< 4) + (c >>> 2)
                        (c <<< 6) + d
                    |]
                | _ -> failwith "Must have between one and four characters"
                |> Seq.take length

        chars
            |> Seq.chunkBySize 4
            |> Seq.collect decodeChunk

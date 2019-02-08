(*
    Hex
    1. encode
    2. decode
    Base 64
    1. encode
    2. decode
*)

namespace Cryptopals

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
    open System

    let private hash = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    let private lookup =
        hash
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
                [|
                    a >>> 2
                    ((a &&& 0b11uy) <<< 4) + (b >>> 4)
                    ((b &&& 0b1111uy) <<< 2) + (c >>> 6)
                    c &&& 0b111111uy
                |]
            | _ -> failwith "Must have between one and three bytes"
            |> Seq.take (Array.length bytes + 1)
            |> Seq.map (fun i -> hash.[int i])
            |> padSequenceRight '=' 4

        bytes
        |> Seq.chunkBySize 3
        |> Seq.collect encodeChunk

    let decode (chars: seq<char>) =
        let getByteOrDefault c = if lookup.ContainsKey c then lookup.[c] else 0b0uy
        let padChunk = padSequenceRight '=' 4 >> Seq.map getByteOrDefault >> Seq.toArray

        let decodeChunk chars =
            match padChunk chars with
            | [|a;b;c;d|] ->
                [|
                    (a <<< 2) + ((b &&& 0b110000uy) >>> 4)
                    (b <<< 4) + (c >>> 2)
                    (c <<< 6) + d
                |]
            | _ -> failwith "Must have between one and four characters"
            |> Seq.take (Array.length chars * 6 / 8)

        chars
        // exclude whitespace characters
        |> Seq.filter (Char.IsWhiteSpace >> not)
        |> Seq.chunkBySize 4
        |> Seq.collect decodeChunk

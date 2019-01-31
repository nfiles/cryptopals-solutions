namespace Cryptopals
open System

module Comparison =
    open System.Collections.Generic

    type FrequencyMap = IDictionary<char, float>

    let buildCorpus (text: string): FrequencyMap =
        let length = text.Length
        text
            |> Seq.countBy id
            |> Seq.map (fun (ch, count) -> (ch, (float count) / (float length)))
            |> dict

    let scoreByCharacterFrequency (corpus: FrequencyMap) (stream: seq<char>) =
        let getExpectedFreq c =
            if corpus.ContainsKey(c)
            then corpus.[c]
            else float 0

        let totalNumChars = stream |> Seq.length

        stream
            // sum the frequency scores of each character
            |> Seq.sumBy getExpectedFreq
            // divide total score by size of the set
            |> fun f -> f / (float totalNumChars)

    let findBestXorByFrequency (corpus: FrequencyMap) (options: seq<byte>) (source: seq<byte>) =
        /// Calculates the XOR of the stream and gets the frequency score
        let xorAndScore (b: byte) =
            Ciphers.xorStreamWithSingleByte b
            >> Seq.map char
            >> scoreByCharacterFrequency corpus

        options
            |> Seq.map (fun key -> (key, xorAndScore key source))
            |> Seq.sortByDescending (fun (_, score) -> score)
            |> Seq.head

    let getHammingDistance (stream1: seq<byte>) (stream2: seq<byte>) =
        let getBit b pos =
            (b >>> pos) &&& 0b00000001uy
            |> (=) (byte 1)
        let getBits b = seq { 0..7 } |> Seq.map (getBit b)

        Seq.zip
            <| Seq.collect getBits stream1
            <| Seq.collect getBits stream2
        |> Seq.filter (fun (a, b) -> a <> b)
        |> Seq.length

    let findBestRepeatingXorKey (corpus: FrequencyMap) (stream: byte[]) =
        let totalLength = Seq.length stream

        /// get the nth chunk of a certain size from the byte sequence
        let getChunk num size (stream: byte[]) =
            let chunkStart = num * size
            let chunkEnd = Math.Min((num + 1) * size, stream.Length)

            if stream.Length < chunkStart
            then [||]
            else stream
                |> Seq.skip chunkStart
                |> Seq.take (chunkEnd - chunkStart)
                |> Seq.toArray
        
        // printfn "chunks:\n%s" ()

        // 1. find the key length
        let possibleKeyLengths = seq { 2..40 }
        let bestKeyLength =
            possibleKeyLengths
            // don't consider keys longer than the input
            |> Seq.filter (fun length -> length < totalLength)
            |> Seq.map (fun length ->
                let chunk1 = getChunk 0 length stream
                let chunk2 = getChunk 1 length stream
                let distance = getHammingDistance chunk1 chunk2
                (distance, length)
            )
            |> Seq.sortBy (fun (distance, length) -> float distance / float length)
            |> Seq.head
            |> fun (_, length) -> length

        printfn "best key length: %d" bestKeyLength

        // 2. find the best key
        let numChunks = (float totalLength) / (float bestKeyLength) |> Math.Ceiling |> int
        let allChunks =
            Seq.map
                <| fun num -> getChunk num bestKeyLength stream |> Seq.toArray
                <| seq { 0..numChunks }

        // create a collection of chunks of the nth byte of each chunk
        let verticalSlices =
            seq { 0..numChunks }
            |> Seq.map (fun num ->
                allChunks
                |> Seq.filter (fun chunk -> num < chunk.Length)
                |> Seq.map (fun chunk -> chunk.[num])
                // |> Seq.map (fun chunk ->
                //     if num >= chunk.Length then None
                //     else Some(chunk.[num])
                // )
                // |> Seq.filter Option.isSome
                // |> Seq.map (fun option ->
                //     match option with
                //     | Some(b) -> b
                //     | _ -> failwith "expected byte value"
                // )
                |> Seq.toArray
            )

        let bestKey =
            verticalSlices
            |> Seq.map (fun slice ->
                findBestXorByFrequency
                    <| corpus
                    <| seq { (byte 0)..(byte 255) }
                    <| slice
                |> fst
            )
            |> Seq.toArray
        
        bestKey

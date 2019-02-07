namespace Cryptopals
open System
open System.Text

module Comparison =
    open System.Collections.Generic

    type FrequencyMap<'T> = IDictionary<'T, float>

    let buildCorpus (text: byte[]): FrequencyMap<byte> =
        let length = text.Length
        text
            |> Seq.countBy id
            |> Seq.map (fun (ch, count) -> (ch, (float count) / (float length)))
            |> dict

    let scoreByCharacterFrequency (corpus: FrequencyMap<byte>) (stream: seq<byte>) =
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

    let findBestXorByFrequency (corpus: FrequencyMap<byte>) (options: seq<byte>) (source: seq<byte>) =
        /// Calculates the XOR of the stream and gets the frequency score
        let xorAndScore (b: byte) =
            Ciphers.xorStreamWithSingleByte b
            >> scoreByCharacterFrequency corpus

        options
            |> Seq.map (fun key -> (key, xorAndScore key source))
            |> Seq.maxBy (fun (_, score) -> score)

    let getHammingDistanceByte (a: byte) (b: byte) =
        let getBit b pos =
            (b >>> pos) &&& 0b00000001uy
            |> (=) (byte 1)
        let getBits b = seq { 0..7 } |> Seq.map (getBit b)
        // XOR produces a byte with 0 for matching bits and 1 for different bits
        getBits (a ^^^ b)
            // count the bits that are different
            |> Seq.filter ((=) true)
            |> Seq.length

    let getHammingDistanceStream (stream1: seq<byte>) (stream2: seq<byte>) =
        Seq.zip stream1 stream2
            |> Seq.sumBy (fun x -> x ||> getHammingDistanceByte)

    /// get the nth chunk of a certain size from the byte sequence
    let getChunk<'T> num size (stream: 'T[]) =
        let chunkStart = num * size
        let chunkEnd = (num + 1) * size - 1
        let chunkEnd = Math.Min(chunkEnd, stream.Length-1)

        if stream.Length < chunkStart
        then [||]
        else stream.[chunkStart..chunkEnd]

    let findRepeatingXorKeyLengths (stream: byte[]) =
        let totalLength = Array.length stream

        // 1. find the key length
        // to get a better average, we want to examine a consistent multiple of the key length
        let maxKeyLength = 40
        let keyLengthMultiplier = Math.Min(totalLength / maxKeyLength, 3)
        let possibleKeyLengths = seq { 2..maxKeyLength }
        let keyLengths =
            possibleKeyLengths
            // don't consider keys longer than the input
            |> Seq.filter (fun length -> length < totalLength)
            |> Seq.map (fun length ->
                // get the hamming distance of the first two chunks
                let chunk1 = getChunk 0 (length * keyLengthMultiplier) stream
                let chunk2 = getChunk 1 (length * keyLengthMultiplier) stream
                let distance = getHammingDistanceStream chunk1 chunk2
                let normalizedDistance = float distance / float length
                (normalizedDistance, length)
            )
            // get the 10 with the smallest hamming distance
            |> Seq.sortBy fst
            |> Seq.take 10
            |> Seq.toArray

        keyLengths

    let compareRepeatingXorKeys (corpus: FrequencyMap<byte>) (possibleKeys: seq<byte[]>) (stream: byte[]) =
        // score each key against the corpus/stream and return the best one
        possibleKeys
        |> Seq.map (fun key ->
            let score = scoreByCharacterFrequency corpus stream
            (score, key)
        )
        |> Seq.maxBy fst

    let getVerticalSlices<'T> (stream: 'T[]) rowLength =
        seq { 0..rowLength-1 }
        |> Seq.map (fun col ->
            Seq.initInfinite (fun row -> row * rowLength + col)
            |> Seq.takeWhile (fun idx -> idx < stream.Length)
            |> Seq.map (fun idx -> stream.[idx])
            |> Seq.toArray
        )
        |> Seq.toArray

    // TODO: make this take a set of key lengths or add another binding
    let findRepeatingXorKey (corpus: FrequencyMap<byte>) (stream: byte[]) (keyLength: int) =
        // 2. find the best key
        /// collection of n slices, each with the nth byte of each chunk
        let verticalSlices = getVerticalSlices stream keyLength

        // printfn "num vertical slices: %3d" verticalSlices.Length

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

        // printfn "----------------\nkeylength: %3d\n%s" keyLength (Encoding.ASCII.GetString bestKey)
        bestKey

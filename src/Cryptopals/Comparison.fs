namespace Cryptopals

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

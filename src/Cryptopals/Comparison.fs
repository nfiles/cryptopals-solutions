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

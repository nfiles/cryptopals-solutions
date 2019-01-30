namespace Cryptopals

module Comparison =
    open System.Collections.Generic
    open System.IO

    type FrequencyMap = IDictionary<char, float>

    let buildCorpus (text: string) =
        let length = text.Length
        text
            |> Seq.countBy id
            |> Seq.map (fun (ch, count) -> (ch, (float count) / (float length)))
            |> dict
        
    let xorStreams streamA streamB =
        let xorBytePair (a: byte, b: byte) = a ^^^ b
        Seq.zip streamA streamB |> Seq.map xorBytePair

    let xorStreamWithSingleByte b = Seq.initInfinite (fun _ -> b) |> xorStreams

    let scoreByCharacterFrequency (corpus: FrequencyMap) (stream: seq<char>) =
        let filteredStream = stream

        let getExpectedFreq c =
            if corpus.ContainsKey(c)
            then corpus.[c]
            else float 0

        let totalNumChars = filteredStream |> Seq.length

        filteredStream
            // count the occurrences of each character
            |> Seq.countBy id
            |> Seq.sumBy (fun (ch, count) ->
                let expected = getExpectedFreq ch
                expected * (float count)
            )
            // divide total score by size of the set
            |> fun f -> f / (float totalNumChars)

    let findBestXorByFrequency (corpus: FrequencyMap) (options: seq<byte>) (source: seq<byte>) =
        /// Calculates the XOR of the stream and gets the frequency "score"
        let xorAndScore (b: byte) =
            xorStreamWithSingleByte b
            >> Seq.map char
            >> scoreByCharacterFrequency corpus

        options
            |> Seq.map (fun key -> (key, xorAndScore key source))
            |> Seq.sortByDescending (fun (_, score) -> score)
            |> Seq.head
            |> fun (key, _) -> key

namespace Cryptopals

module Comparison =
    open System

    let private padSequence pad length source =
        [source; Seq.initInfinite (fun _ -> pad)] |> Seq.take length

    let xorStreams streamA streamB =
        let xorBytePair (a:byte, b:byte) = a ^^^ b
        Seq.zip streamA streamB |> Seq.map xorBytePair

    let xorStreamWithSingleByte b = Seq.initInfinite (fun _ -> b) |> xorStreams

    let characterFrequencyInStream stream =
        let frequency all occurrences = double (Seq.length occurrences) / double (Seq.length all)
        let countOccurrences (key, occurrences) = (key, frequency stream occurrences)
        stream |> Seq.groupBy Char.ToUpper |> Seq.map countOccurrences |> dict

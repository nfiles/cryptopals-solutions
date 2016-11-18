namespace Cryptopals

module Comparison =
    open System

    let private padSequence pad length source =
        [source; Seq.initInfinite (fun _ -> pad)] |> Seq.take length

    let private charToByte (c:char) = Text.Encoding.UTF8.GetBytes(string c) |> Seq.exactlyOne
    let private byteToChar (b:byte) = Text.Encoding.UTF8.GetChars([|b|]) |> Seq.exactlyOne

    let xorStreams streamA streamB =
        let xorBytePair (a:byte, b:byte) = a ^^^ b
        Seq.zip streamA streamB |> Seq.map xorBytePair

    let xorStreamWithSingleByte b = Seq.initInfinite (fun _ -> b) |> xorStreams

    let characterFrequenciesInStream stream =
        let frequency all occurrences = float (Seq.length occurrences) / float (Seq.length all)
        let countOccurrences (key, occurrences) = (key, frequency stream occurrences)
        stream |> Seq.groupBy Char.ToUpper |> Seq.map countOccurrences

    let alphanumericCharacters =
        [['a'..'z'];['A'..'Z'];['0'..'9']] |> Seq.concat |> set
    let acceptedCharacters =
        [
            seq alphanumericCharacters
            seq " ,'\"@.?!"
        ] |> Seq.concat |> set
    let predictedFrequencies =
        // English character frequency taken from Algoritmy.net
        // ref: http://en.algoritmy.net/article/40379/Letter-frequency-English
        // alt: https://en.wikipedia.org/wiki/Letter_frequency
        [
            ('A', 8.167 / 100.0)
            ('B', 1.492 / 100.0)
            ('C', 2.782 / 100.0)
            ('D', 4.253 / 100.0)
            ('E', 12.702 / 100.0)
            ('F', 2.228 / 100.0)
            ('G', 2.015 / 100.0)
            ('H', 6.094 / 100.0)
            ('I', 6.966 / 100.0)
            ('J', 0.153 / 100.0)
            ('K', 0.772 / 100.0)
            ('L', 4.025 / 100.0)
            ('M', 2.406 / 100.0)
            ('N', 6.749 / 100.0)
            ('O', 7.507 / 100.0)
            ('P', 1.929 / 100.0)
            ('Q', 0.095 / 100.0)
            ('R', 5.987 / 100.0)
            ('S', 6.327 / 100.0)
            ('T', 9.056 / 100.0)
            ('U', 2.758 / 100.0)
            ('V', 0.978 / 100.0)
            ('W', 2.360 / 100.0)
            ('X', 0.150 / 100.0)
            ('Y', 1.974 / 100.0)
            ('Z', 0.074 / 100.0)
        ]

    let predictedFrequenciesLookup = predictedFrequencies |> dict
    // let isExpectedChar = Char.ToUpper >> predictedFrequenciesLookup.ContainsKey

    let scoreCharacterFrequency (stream:char seq) =
        let calculateFrequencyDifference (c,f) =
            Math.Pow(f - predictedFrequenciesLookup.[c] |> abs, 2.0)

        let validChars =
            stream
            |> Seq.map Char.ToUpper
            |> Seq.filter predictedFrequenciesLookup.ContainsKey
            |> List.ofSeq

        let validCharScore =
            stream
            |> Seq.map (fun c ->
                if alphanumericCharacters.Contains(c) then 2
                else if acceptedCharacters.Contains(c) then 1
                else 0
            )
            |> Seq.sum

        let frequencyScore =
            validChars
            |> characterFrequenciesInStream
            |> Seq.map calculateFrequencyDifference
            |> Seq.sum

        frequencyScore * float validCharScore // (List.length validChars |> float)

    let findBestXorByFrequency (options:byte seq) (source:byte seq) =
        let xorAndScore (b:byte) =
            xorStreamWithSingleByte b
            >> Seq.map char
            >> scoreCharacterFrequency

        let x =
            options
            |> Seq.map (fun b -> (b, xorAndScore b source))
            |> Seq.sortBy (fun (b,f) -> f)

        Seq.iter
            <| fun (b,f) ->
                printfn "score: %f char: %s option: %s"
                    <| f
                    <| (b |> byteToChar |> string)
                    <| (xorStreamWithSingleByte b source
                        |> Seq.map (byteToChar >> string)
                        |> String.concat ""
                    )
            <| x

        x
            |> Seq.head
            |> fun (b,f) -> (byteToChar b)

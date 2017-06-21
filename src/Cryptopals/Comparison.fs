namespace Cryptopals

module Comparison =
    open System

    let xorStreams streamA streamB =
        let xorBytePair (a:byte, b:byte) = a ^^^ b
        Seq.zip streamA streamB |> Seq.map xorBytePair

    let xorStreamWithSingleByte b = Seq.initInfinite (fun _ -> b) |> xorStreams

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
    
    /// Characters that should be ignored when calculating frequency score
    let validPunctuation = " ,'\"@.?!"

    let scoreByCharacterFrequency (stream:char seq) =
        // ignore all valid punctuation. We don't have scores for these
        let filteredStream =
            Seq.filter
                <| (fun ch ->
                    Seq.contains ch validPunctuation |> not
                )
                <| stream

        let getExpectedFreq c =
            if predictedFrequenciesLookup.ContainsKey(c)
            then predictedFrequenciesLookup.[c]
            else float 0

        let totalNumChars = filteredStream |> Seq.length

        /// find the number of occurences of each character
        let getFrequencyScore =
            // count (case-insensitive) the occurrences of each character
            Seq.countBy Char.ToUpper
            // find the total variance of the set
            >> Seq.sumBy (fun (ch, count) ->
                let expected = getExpectedFreq ch
                let actual = float count / float totalNumChars
                // square the difference between expected and actual
                Math.Pow(expected - actual, 2.0)
            )
            // divide total variance by size of the set
            >> fun f -> f / (float totalNumChars)

        filteredStream |> getFrequencyScore

    let findBestXorByFrequency (options:byte seq) (source:byte seq) =
        /// Calculates the XOR of the stream and gets the frequency "score"
        let xorAndScore (b:byte) =
            xorStreamWithSingleByte b
            >> Seq.map char
            >> scoreByCharacterFrequency

        options
            |> Seq.map (fun b -> (b, xorAndScore b source))
            |> Seq.sortBy (fun (b,f) -> f)
            |> Seq.head
            |> fun (b,f) -> b

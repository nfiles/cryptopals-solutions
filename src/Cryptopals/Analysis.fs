namespace Cryptopals

open System
open System.Text

module Analysis =
    open System.Collections.Generic

    /// Expected frequencies of elements
    type FrequencyMap<'T> = IDictionary<'T, float>

    let private allBits = seq { 0..7 }
    let private allBytes = seq { (byte 0)..(byte 255) }

    /// build a corpus for frequency analysis by scoring a sample
    let buildCorpus text: FrequencyMap<'T> =
        let length = Array.length text
        text
        |> Seq.countBy id
        |> Seq.map (fun (ch, count) -> (ch, (float count) / (float length)))
        |> dict

    /// score how closely the elements of the stream match the expected frequencies
    let scoreByCharacterFrequency (corpus: FrequencyMap<'T>) stream =
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

    /// use frequency analysis to find the best key for an XOR cipher
    let findBestXorByFrequency corpus options source =
        /// Calculates the XOR of the stream and gets the frequency score
        let xorAndScore b =
            Ciphers.xorStreamWithSingleByte b
            >> scoreByCharacterFrequency corpus

        options
        |> Seq.map (fun key -> (key, xorAndScore key source))
        |> Seq.maxBy snd

    /// get the hamming distance between two bytes
    let getHammingDistanceByte a b =
        let getBit b pos =
            (b >>> pos) &&& 0b00000001uy
            |> (=) (byte 1)
        let getBits b = allBits |> Seq.map (getBit b)
        // XOR produces a byte with 0 for matching bits and 1 for different bits
        getBits (a ^^^ b)
        // count the bits that are different
        |> Seq.filter ((=) true)
        |> Seq.length

    /// get the hamming distance between two byte streams
    let getHammingDistanceStream stream1 stream2 =
        Seq.zip stream1 stream2
        |> Seq.sumBy (fun x -> x ||> getHammingDistanceByte)

    /// get the nth chunk of a certain size from the byte sequence
    let getChunk num size (stream: 'T[]) =
        let chunkStart = num * size
        let chunkEnd = (num + 1) * size - 1
        let chunkEnd = Math.Min(chunkEnd, stream.Length-1)

        if stream.Length < chunkStart
        then [||]
        else stream.[chunkStart..chunkEnd]

    /// find the top 20% most likely key sizes for a repeating XOR cipher
    let findLikelyRepeatingXorKeyLengths stream =
        let totalLength = Array.length stream

        // to get a better average, we want to examine a consistent multiple of the key length
        let maxKeyLength = 40
        let keyLengthMultiplier = Math.Min(totalLength / maxKeyLength, 3)
        let possibleKeyLengths = seq { 2..maxKeyLength } |> Seq.toArray

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
        // get the top 20% with the smallest hamming distance
        |> Seq.sortBy fst
        |> Seq.take (possibleKeyLengths.Length / 5)
        |> Seq.map snd
        |> Seq.toArray

    /// build a collection of n slices, each with the nth byte of each chunk
    let getVerticalSlices stream rowLength =
        seq { 0..rowLength-1 }
        |> Seq.map (fun col ->
            Seq.initInfinite (fun row -> row * rowLength + col)
            |> Seq.takeWhile (fun idx -> idx < Array.length stream)
            |> Seq.map (fun idx -> stream.[idx])
        )

    /// Find the most likely key for a repeating XOR cipher, given a set of possible key lengths
    let findRepeatingXorKey corpus stream keyLengths =
        keyLengths
        |> Seq.map (fun keyLength ->
            let verticalSlices = getVerticalSlices stream keyLength
            let findSingleByteXorKey = findBestXorByFrequency corpus allBytes >> fst

            let key = verticalSlices |> Seq.map findSingleByteXorKey

            let score = scoreByCharacterFrequency corpus stream
            (score, key)
        )
        |> Seq.maxBy fst
        |> snd

    let detectRepeatedBlock size input =
        let mutable found = new Set<string> [||]
        Seq.chunkBySize size input
        |> Seq.map Encoding.ASCII.GetString
        |> Seq.exists (fun block ->
            if found.Contains block then
                true
            else
                found <- found.Add block
                false
        )

    let ecbCBCOracle key payload =
        let prefixLength = Utilities.randomRange 5 11
        let suffixLength = Utilities.randomRange 5 11

        let msg =
            seq {
                yield! Utilities.randomBytes prefixLength
                yield! payload
                yield! Utilities.randomBytes suffixLength
            }
            |> Seq.toArray
            |> Ciphers.padBlockPKCS7 (Array.length key)

        let isECB = Utilities.randomRange 0 2 = 0

        let encrypted =
            if isECB then
                Ciphers.encryptECB key msg
            else
                let iv = Utilities.randomBytes key.Length
                Ciphers.encryptCBC key iv payload

        (encrypted, isECB)

    let ecbSuffixOracle secretSuffix key payload =
        Array.append payload secretSuffix
        |> Ciphers.padBlockPKCS7 (Array.length key)
        |> Ciphers.encryptECB key

    let recoverEcbSuffix (oracle: byte[] -> byte[]) =
        // find the first block size that repeats in the first two chunks
        let blockSize =
            seq { 2 .. 100 }
            |> Seq.find (fun bs ->
                Array.create (bs * 2) 69uy
                |> oracle
                |> Seq.take (bs * 2)
                |> detectRepeatedBlock bs)

        let getBlock offset size input =
            Array.sub input offset size
            |> Hex.encode
            |> Seq.toArray
            |> String

        /// Create a dictionary of all results from calling the oracle with an input of
        /// [ padding * (known.Length % blockSize - 1), known, X ], where X is every possible byte from 0-255
        let findAllByteOptions blockOffset blockSize known =
            let padLength = blockSize - (Array.length known % blockSize) - 1
            let padding = Array.zeroCreate padLength
            let msg =
                [| yield! padding
                   yield! known
                   yield 0uy |]

            let allOptions =
                seq {
                    for b = 0uy to 255uy do
                        msg.[msg.Length - 1] <- b
                        let block = oracle msg |> getBlock blockOffset blockSize

                        yield (block, b)
                }
                |> dict

            (padding, allOptions)

        /// length of the secret suffix in the oracle
        let secretLength =
            let totalLength = oracle Array.empty |> Array.length

            /// number of bytes of padding
            let diff =
                Seq.find
                    <| fun n ->
                        let padding = Array.zeroCreate n
                        let length = oracle padding |> Array.length
                        length > totalLength
                    <| seq { 1 .. blockSize }

            totalLength - diff

        let plaintext =
            Array.fold
                <| (fun (known: byte[]) i ->
                    let blockOffset = i / blockSize * blockSize
                    let (padding, options) = findAllByteOptions blockOffset blockSize known

                    let lastBlock = oracle padding |> getBlock blockOffset blockSize

                    let knownByte = options.Item lastBlock

                    [| yield! known
                       yield knownByte |])
                <| Array.empty
                <| [| 0 .. (secretLength - 1) |]

        plaintext

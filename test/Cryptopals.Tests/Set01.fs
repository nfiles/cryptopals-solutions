namespace Cryptopals.Tests

open System
open System.Collections.Generic
open System.IO
open System.Text
open Cryptopals
open Xunit
open Xunit.Abstractions

module Set01Tests =
    let hexToBase64String = Hex.decode >> Base64.encode >> String.Concat

    /// create a character frequency map from the sample text
    let corpus = 
        File.ReadAllText "./data/aliceinwonderland.txt"
        |> Comparison.buildCorpus
 
    let private possibleKeys = seq { (byte 0)..(byte 255) }

    [<Fact>]
    let Challenge01ConvertHexToBase64() =
        let input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        let expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

        let actual = hexToBase64String input
        Assert.Equal(expected, actual, StringComparer.OrdinalIgnoreCase)

    [<Fact>]
    let Challenge02FixedXor() =
        let input = "1c0111001f010100061a024b53535009181c"
        let xorTarget = "686974207468652062756c6c277320657965"
        let expected = "746865206b696420646f6e277420706c6179"

        let actual =
            Comparison.xorStreams
                <| Hex.decode input
                <| Hex.decode xorTarget
            |> Hex.encode
            |> String.Concat

        Assert.Equal(expected, actual, StringComparer.OrdinalIgnoreCase)

    [<Fact>]
    let Challenge03SingleByteXORCipher() =
        let input = Hex.decode "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
        let expected = "Cooking MC's like a pound of bacon"

        let actualKey = Comparison.findBestXorByFrequency corpus possibleKeys input

        let cleartext =
            Comparison.xorStreamWithSingleByte
                <| actualKey
                <| input
            |> Seq.map char
            |> String.Concat

        Assert.Equal(expected, cleartext)

    [<Fact>]
    let Challenge04DetectSingleCharacterXOR() =
        let input =
            File.ReadAllLines "./data/set01/4.txt"
            |> Seq.map Hex.decode
        // not sure why the newline character is at the end, but it is
        let expected = "Now that the party is jumping\n"

        // read all the data and calculate a score for each row
        let actual =
            input
            |> Seq.collect (fun raw ->
                possibleKeys |> Seq.map (fun key -> (key, raw))
            )
            |> Seq.map (fun (key, raw) ->
                let clear = 
                    Comparison.xorStreamWithSingleByte key raw
                    |> Seq.map char
                    |> String.Concat
                let score = Comparison.scoreByCharacterFrequency corpus clear
                (score, raw, clear)
            )
            |> Seq.sortByDescending (fun (score, _, _) -> score)
            |> Seq.head
            |> fun (_, _, clear) -> clear

        Assert.Equal(expected, actual)

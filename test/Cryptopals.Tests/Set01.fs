namespace Cryptopals.Tests

open Xunit
open System
open System.Collections.Generic
open Cryptopals

module Set01Tests =
    let private hexToBase64String = Hex.decode >> Base64.encode >> String.Concat

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

        let possibleKeys =
            [
                ['a'..'z']
                ['A'..'Z']
                ['0'..'9']
            ]
            |> Seq.collect (
                String.Concat >> System.Text.Encoding.UTF8.GetBytes
            )

        let actualKey = Comparison.findBestXorByFrequency possibleKeys input

        let cleartext =
            Comparison.xorStreamWithSingleByte
                <| actualKey
                <| input
            |> Seq.map char
            |> String.Concat

        Assert.Equal(expected, cleartext)

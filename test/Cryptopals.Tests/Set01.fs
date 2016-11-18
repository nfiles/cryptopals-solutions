namespace Cryptopals.Tests

open Xunit
open System
open System.Collections.Generic
open Cryptopals

module Set01Tests =
    let private hexToBase64String = seq >> Hex.decode >> Base64.encode >> TestHelpers.seqToString

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
        let expected = seq "746865206b696420646f6e277420706c6179"

        let actual =
            Comparison.xorStreams (Hex.decode input) (Hex.decode xorTarget)
                |> Hex.encode

        Assert.Equal(expected |> TestHelpers.seqToString,
                     actual   |> TestHelpers.seqToString,
                     StringComparer.OrdinalIgnoreCase)

    [<Fact>]
    let Challenge03SingleByteXORCipher() =
        let input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

        let bytes = Hex.decode input

        let byteToChar (b:byte) = System.Text.Encoding.UTF8.GetChars([|b|]).[0]
        let charToByte (c:char) = System.Text.Encoding.UTF8.GetBytes([|c|]).[0]
        printfn "char-ed input bytes: %s" (bytes |> Seq.map (byteToChar >> string) |> String.concat "")

        let options =
            [['a'..'z'];['A'..'Z'];['0'..'9']]
            |> Seq.concat |> Seq.map charToByte

        printfn "options: %s" (options |> Seq.map (byteToChar >> string) |> String.concat "")
        let c = Comparison.findBestXorByFrequency options bytes
        printfn "Best XOR character: %s" (string c)

        let cleartext =
            Comparison.xorStreamWithSingleByte (byte c) bytes
                |> Seq.map char
                |> TestHelpers.seqToString
        printfn "cleartext: %s" cleartext

module ComparisonTests =
    let characterFrequencyData: Object [] list =
        [
            [|
                "hello"
                [('H',0.2);('E',0.2);('L',0.4);('O',0.2)] |> dict
            |]
            [|
                "tenletters"
                [('T',0.3);('E',0.3);('N',0.1);('L',0.1);('S',0.1);('R',0.1)] |> dict
            |]
        ]

    [<Theory>]
    [<MemberData "characterFrequencyData">]
    let CharacterFrequency(input, expected:IDictionary<char,double>) =
        let getKey (i:KeyValuePair<_,_>) = i.Key
        let sortDictionary = seq >> Seq.sortBy (getKey >> Char.ToUpper)
        let actual = Comparison.characterFrequenciesInStream input |> dict

        Seq.zip (sortDictionary expected) (sortDictionary actual)
            |> Seq.iter (fun (a,b) -> Assert.Equal(a,b))

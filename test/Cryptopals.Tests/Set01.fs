namespace Cryptopals.Tests

open Xunit
open System
open System.Collections.Generic
open Cryptopals

module Set01Tests =
    let private hexToBase64String = seq >> Hex.decode >> Base64.encode >> TestHelpers.seqToString

    [<Fact>]
    let ConvertHexToBase64() =
        let input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        let expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

        let actual = hexToBase64String input
        Assert.Equal(expected, actual, StringComparer.OrdinalIgnoreCase)

    [<Fact>]
    let FixedXor() =
        let input = "1c0111001f010100061a024b53535009181c"
        let xorTarget = "686974207468652062756c6c277320657965"
        let expected = "746865206b696420646f6e277420706c6179"

        let actual =
            Comparison.xorStreams (Hex.decode input) (Hex.decode xorTarget)
                |> Hex.encode
                |> TestHelpers.seqToString

        Assert.Equal(expected, actual, StringComparer.OrdinalIgnoreCase)

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
        let actual = Comparison.characterFrequencyInStream input

        Seq.zip (sortDictionary expected) (sortDictionary actual)
            |> Seq.iter (fun (a,b) -> Assert.Equal(a,b))

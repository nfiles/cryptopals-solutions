namespace Cryptopals.Tests

module Set01Tests =
    open Xunit
    open System
    open Encoding
    open Cryptopals

    let private hexToBase64String = seq >> Hex.decode >> Base64.encode >> TestHelpers.seqToString

    [<Fact>]
    let ConvertHexToBase64() =
        let input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        let expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

        let actual = hexToBase64String input
        Assert.Equal(expected, actual, StringComparer.OrdinalIgnoreCase)

    let private xorAgainst seqA seqB =
        Comparison.xorStreams
            <| seq seqA
            <| seq seqB

    [<Fact>]
    let FixedXor() =
        let input = "1c0111001f010100061a024b53535009181c"
        let xorTarget = "686974207468652062756c6c277320657965"
        let expected = "746865206b696420646f6e277420706c6179"

        let actual =
            xorAgainst (Hex.decode input) (Hex.decode xorTarget)
                |> Hex.encode
                |> TestHelpers.seqToString

        Assert.Equal(expected, actual, StringComparer.OrdinalIgnoreCase)

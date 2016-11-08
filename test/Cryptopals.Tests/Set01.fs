namespace Cryptopals.Tests

module Set01Tests =
    open Xunit
    open System
    open Encoding

    let private hexToBase64String = seq >> Hex.decode >> Base64.encode >> TestHelpers.seqToString

    [<Fact>]
    let ConvertHexToBase64() =
        let input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        let expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

        let actual = hexToBase64String input
        Assert.Equal(expected, actual, StringComparer.OrdinalIgnoreCase)

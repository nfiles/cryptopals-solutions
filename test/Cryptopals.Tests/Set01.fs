namespace Cryptopals.Tests

open System
open System.IO
open System.Text
open Cryptopals
open Xunit
open FSharp.Reflection

module Set01Tests =
    let hexToBase64String = Hex.decode >> Base64.encode >> String.Concat

    /// create a character frequency map from the sample text
    let corpus =
        File.ReadAllBytes "./data/aliceinwonderland.txt"
        |> Analysis.buildCorpus

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
            Ciphers.xorStreams
                <| Hex.decode input
                <| Hex.decode xorTarget
            |> Hex.encode
            |> String.Concat

        Assert.Equal(expected, actual, StringComparer.OrdinalIgnoreCase)

    [<Fact>]
    let Challenge03SingleByteXORCipher() =
        let input = Hex.decode "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
        let expected = "Cooking MC's like a pound of bacon"

        let actualKey = Analysis.findBestXorByFrequency corpus possibleKeys input |> fst

        let plaintext =
            Ciphers.xorStreamWithSingleByte
                <| actualKey
                <| input
            |> Seq.map char
            |> String.Concat

        Assert.Equal(expected, plaintext)

    [<Fact>]
    let Challenge04DetectSingleCharacterXOR() =
        let input =
            File.ReadAllLines "./data/set01/4.txt"
            |> Seq.map Hex.decode
        // not sure why the newline character is at the end, but it is
        let expected = "Now that the party is jumping\n"

        // read all the data and calculate a score for each row
        let _, actual =
            input
            |> Seq.map (fun raw ->
                let key, score = Analysis.findBestXorByFrequency corpus possibleKeys raw
                let clear =
                    Ciphers.xorStreamWithSingleByte key raw
                    |> Seq.map char
                    |> String.Concat
                (score, clear)
            )
            |> Seq.sortByDescending (fun (score, _) -> score)
            |> Seq.head

        Assert.Equal(expected, actual)

    [<Fact>]
    let Challenge05RepeatingKeyXOR() =
        let input = "Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal"
        let key = "ICE"
        let expected =
            "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272\
a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

        let actual =
            Ciphers.xorStreamWithRepeatingKey
                <| Encoding.ASCII.GetBytes key
                <| Encoding.ASCII.GetBytes input
            |> Hex.encode
            |> String.Concat

        Assert.Equal(expected, actual, StringComparer.OrdinalIgnoreCase)

    [<Fact>]
    let Challenge06HammingDistance() =
        let a = "this is a test" |> Encoding.ASCII.GetBytes
        let b = "wokka wokka!!!" |> Encoding.ASCII.GetBytes
        let expected = 37

        let actual = Analysis.getHammingDistanceStream a b

        Assert.Equal(expected, actual)

    [<Fact>]
    let Challenge06VerticalSlices() =
        let input = [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 |]
        let rowLength = 4
        let expected = [|
            [| 0; 4; 8 |]
            [| 1; 5; 9 |]
            [| 2; 6 |]
            [| 3; 7 |]
        |]

        let actual = Analysis.getVerticalSlices input rowLength |> Seq.toArray

        Assert.Equal(expected.Length, actual.Length)
        for expected, actual in (Seq.zip expected actual) do
            Assert.Equal<int>(expected, actual)

    let GetChunkValues =
        seq {
            yield (0, 4, [| 0; 1; 2; 3 |])
            yield (1, 4, [| 4; 5; 6; 7 |])
            yield (2, 4, [| 8; 9 |])
        }
        |> Seq.map FSharpValue.GetTupleFields

    [<Theory; MemberData("GetChunkValues")>]
    let Challenge06GetChunk(num, size, expected) =
        let input = [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 |]

        let actual = Analysis.getChunk num size input |> Seq.toArray

        Assert.Equal<int>(expected, actual)

    [<Fact>]
    let Challenge06BreakRepeatingKeyXOR() =
        let input =
            File.ReadAllText "./data/set01/6.txt"
            |> Base64.decode
            |> Seq.toArray
        let expectedKey = "Terminator X: Bring the noise"

        let keyLengths = Analysis.findLikelyRepeatingXorKeyLengths input

        let actualKey =
            Analysis.findRepeatingXorKey
                <| corpus
                <| input
                <| keyLengths
            |> Seq.toArray
            |> Encoding.ASCII.GetString

        Assert.Equal(expectedKey, actualKey)

    [<Fact>]
    let Challenge07EcbDecrypt() =
        let input =
            File.ReadAllText "./data/set01/7.txt"
            |> Base64.decode
            |> Seq.toArray
        let key = Encoding.ASCII.GetBytes "YELLOW SUBMARINE"
        let expected = "I'm back and I'm ringin' the bell"

        let actual = Ciphers.decryptECB key input |> Encoding.ASCII.GetString

        Assert.StartsWith(expected, actual)

    [<Fact>]
    let Challenge07EcbBothWays() =
        let key = Encoding.ASCII.GetBytes "YELLOW SUBMARINE"
        let original = "This is some sample text for encryption. Good luck."

        let actual =
            original
            |> Encoding.ASCII.GetBytes
            |> Ciphers.padBlockPKCS7 key.Length
            |> Ciphers.encryptECB key
            |> Ciphers.decryptECB key
            |> Ciphers.unpadBlockPKCS7
            |> Encoding.ASCII.GetString

        Assert.Equal(original, actual)

    [<Fact>]
    let Challenge08EcbDetect() =
        let input =
            File.ReadAllLines "./data/set01/8.txt"
            |> Seq.map Hex.decode
            |> Seq.toArray
        let expected = 132

        // find the first row with a repeated block
        let actual = Seq.findIndex (Analysis.detectRepeatedBlock 16) input

        Assert.Equal(expected, actual)

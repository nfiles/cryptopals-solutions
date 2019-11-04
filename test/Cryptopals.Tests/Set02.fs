namespace Cryptopals.Tests

open System
open System.IO
open System.Text
open Cryptopals
open Microsoft.FSharp.Reflection
open Xunit

module Set02Tests =
    let GetPaddingValues =
        seq {
            yield (20, 4)
            yield (16, 16)
            yield (10, 4)
            yield (5, 4)
        }
        |> Seq.map FSharpValue.GetTupleFields

    [<Theory; MemberData("GetPaddingValues")>]
    let Challenge09PKCS7Padding(blockSize: int, padNum: int) =
        let input = Encoding.ASCII.GetBytes "YELLOW SUBMARINE"
        let padding = Seq.init padNum (fun _ -> byte padNum)
        let expected = Seq.append input padding |> Seq.toArray

        let actual = Ciphers.padBlockPKCS7 blockSize input

        Assert.Equal<byte>(expected, actual)

    let GetUnpaddingValues =
        seq {
            yield (true, 4, 4)
            yield (true, 16, 16)
            yield (false, 4, 3)
            yield (false, 0, 0)
        }
        |> Seq.map FSharpValue.GetTupleFields

    [<Theory; MemberData("GetUnpaddingValues")>]
    let Challenge09PKCS7Unpad(success: bool, padNum: byte, padLength: int) =
        let input =
            Seq.append
                <| Encoding.ASCII.GetBytes "YELLOW SUBMARINE"
                <| Array.create padLength padNum
            |> Seq.toArray
        let expected = Encoding.ASCII.GetBytes "YELLOW SUBMARINE"

        let action() = Ciphers.unpadBlockPKCS7 input

        if success then
            let actual = action()
            Assert.Equal<byte>(expected, actual)
        else
            Assert.ThrowsAny<Exception>(action >> ignore) |> ignore

    [<Fact>]
    let Challenge10ImplementCBCMode() =
        let original = "Sample text for CBC encryption. Good luck."
        let key = Encoding.ASCII.GetBytes "YELLOW SUBMARINE"
        let iv = key

        let actual =
            original
            |> Encoding.ASCII.GetBytes
            |> Ciphers.padBlockPKCS7 key.Length
            |> Ciphers.encryptCBC key iv
            |> Ciphers.decryptCBC key iv
            |> Ciphers.unpadBlockPKCS7
            |> Encoding.ASCII.GetString

        Assert.Equal(original, actual)

    [<Fact>]
    let Challenge10CBCModeDecrypt() =
        let input =
            File.ReadAllText "./data/set02/10.txt"
            |> Base64.decode
            |> Seq.toArray
        let key = Encoding.ASCII.GetBytes "YELLOW SUBMARINE"
        let iv = Array.create key.Length 0uy
        let expected = "I'm back and I'm ringin' the bell"

        let actual =
            Ciphers.decryptCBC key iv input
            |> Ciphers.unpadBlockPKCS7
            |> Encoding.ASCII.GetString

        Assert.StartsWith(expected, actual)

    [<Fact>]
    let Challenge11RandomRange() =
        let min = 50
        let max = 100
        let tries = 1000

        let plot =
            seq { 0 .. tries }
            |> Seq.map (fun _ -> Utilities.randomRange min max)
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toArray

        Assert.Equal(min, plot.[0])
        Assert.Equal(max - 1, plot.[plot.Length - 1])
        Assert.Equal(max - min, plot.Length)

    [<Fact>]
    let Challenge11RandomBytesShouldBeUnique() =
        let length = 10
        let tries = 100

        let results =
            seq { 1 .. tries }
            |> Seq.map (fun _ ->
                Utilities.randomBytes length
                |> Hex.encode
                |> String.Concat)
            |> Seq.distinct
            |> Seq.toArray

        Assert.Equal(tries, results.Length)

    [<Fact>]
    let Challenge11DetermineEcbOrCbc() =
        let key = Utilities.randomBytes 16
        // we know the key is 16 bytes, so the input should be long enough to have
        // at least two identical blocks regardless of the padding added in the oracle
        let plaintext = Array.create (key.Length * 4) (byte 69)

        let oracle = Analysis.ECBCBCOracle key
        for i = 1 to 10000 do
            let (ciphertext, expectedCbcMode) = oracle plaintext

            let actualCbcMode = Analysis.detectRepeatedBlock key.Length ciphertext

            Assert.Equal(expectedCbcMode, actualCbcMode)

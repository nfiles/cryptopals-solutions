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

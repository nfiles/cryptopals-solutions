namespace Cryptopals.Tests

open System.Text
open Cryptopals
open Xunit

module Set02Tests =
    [<Fact>]
    let Challenge09PKCS7Padding() =
        let input = Encoding.ASCII.GetBytes "YELLOW SUBMARINE"
        let size = 20
        let padding = [| 4; 4; 4; 4 |] |> Seq.map byte
        let expected = Seq.append input padding |> Seq.toArray

        let actual = Ciphers.padBlockPKCS7 size input

        Assert.Equal<byte>(expected, actual)

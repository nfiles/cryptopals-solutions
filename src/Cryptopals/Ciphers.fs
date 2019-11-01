namespace Cryptopals

open System.IO
open System.Security.Cryptography

module Ciphers =
    let xorStreams streamA streamB =
        let xorBytePair (a: byte, b: byte) = a ^^^ b
        Seq.zip streamA streamB |> Seq.map xorBytePair

    let xorStreamWithSingleByte (b: byte) =
        xorStreams <| Seq.initInfinite (fun _ -> b)

    let xorStreamWithRepeatingKey (key: byte []) =
        xorStreams <| Seq.initInfinite (fun i -> key.[i % key.Length])

    let private createEcb key =
        let crypto = Aes.Create();
        crypto.Key <- key
        crypto.Padding <- PaddingMode.None
        crypto.Mode <- CipherMode.ECB

        crypto

    let encryptECB (key: byte[]) (plaintext: byte[]) =
        use crypto = createEcb key

        use encryptor = crypto.CreateEncryptor()
        use msEncrypt = new MemoryStream()
        use csEncrypt = new CryptoStream(msEncrypt, encryptor, CryptoStreamMode.Write)

        csEncrypt.Write(plaintext, 0, plaintext.Length)
        csEncrypt.Flush()
        csEncrypt.FlushFinalBlock()

        msEncrypt.ToArray()

    let decryptECB (key: byte[]) (ciphertext: byte[]) =
        use crypto = createEcb key

        use decryptor = crypto.CreateDecryptor()
        use msDecrypt = new MemoryStream(ciphertext)
        use csDecrypt = new CryptoStream(msDecrypt, decryptor, CryptoStreamMode.Read)
        use msOutput  = new MemoryStream()

        csDecrypt.CopyTo(msOutput)

        msOutput.ToArray()

    let padBlockPKCS7 (blockSize: int) (input: byte[]) =
        if blockSize >= 256 then
            failwith "cannot pad to size higher than 255"

        let paddingSize = blockSize - (input.Length % blockSize)
        let totalSize = paddingSize + input.Length
        let padding = Seq.initInfinite (fun _ -> byte paddingSize)

        Seq.append input padding
        |> Seq.take totalSize
        |> Seq.toArray

    let unpadBlockPKCS7 (input: byte[]) =
        let padByte = input.[input.Length - 1]

        if int padByte >= input.Length then
            failwithf "invalid padding: expected padding length (%d) is longer than input (%d)" padByte input.Length

        let padLength =
            input
            |> Seq.rev
            |> Seq.takeWhile (fun x -> x = padByte)
            |> Seq.length

        if int padByte <> padLength then
            failwithf "invalid padding: padding byte (%d) does not equal padding length (%d)" padByte padLength

        input
        |> Seq.take (input.Length - padLength)
        |> Seq.toArray

    let encryptCBC (key: byte[]) (iv: byte[]) (plaintext: byte[]) =
        if plaintext.Length % key.Length <> 0 then
            failwithf "plaintext length (%d) must be a multiple of the key length (%d)" plaintext.Length key.Length

        if key.Length <> iv.Length then
            failwithf "length of key (%d) and iv (%d) must be the same" key.Length iv.Length

        let chunks = Seq.chunkBySize key.Length plaintext
        let mutable prev = iv
        seq {
            for plaintext in chunks do
                let xord = xorStreams plaintext prev
                let encrypted = encryptECB key (Seq.toArray xord)
                prev <- encrypted
                yield! prev
        }
        |> Seq.toArray

    let decryptCBC (key: byte[]) (iv: byte[]) (ciphertext: byte[]) =
        if ciphertext.Length % key.Length <> 0 then
            failwithf "ciphertext length (%d) must be a multiple of the key length (%d)" ciphertext.Length key.Length

        if key.Length <> iv.Length then
            failwithf "length of key (%d) and iv (%d) must be the same" key.Length iv.Length

        let chunks = Seq.chunkBySize key.Length ciphertext
        let mutable prev = iv
        seq {
            for ciphertext in chunks do
                let decrypted = decryptECB key ciphertext
                let xord = xorStreams decrypted prev
                prev <- ciphertext
                yield! xord
        }
        |> Seq.toArray

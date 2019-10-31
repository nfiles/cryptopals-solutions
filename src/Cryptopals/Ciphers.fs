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

    let decryptECB (ciphertext: byte[]) (key: byte[]) =
        use crypto = Aes.Create();
        crypto.Key <- key
        crypto.BlockSize <- key.Length * 8
        crypto.Padding <- PaddingMode.PKCS7
        crypto.Mode <- CipherMode.ECB

        let decryptor = crypto.CreateDecryptor()
        use msDecrypt = new MemoryStream(ciphertext)
        use csDecrypt = new CryptoStream(msDecrypt, decryptor, CryptoStreamMode.Read)
        use srDecrypt = new StreamReader(csDecrypt)

        csDecrypt.Flush()
        msDecrypt.Flush()

        srDecrypt.ReadToEnd()

    let encryptECB (plaintext: string) (key: byte[]) =
        use crypto = Aes.Create()
        crypto.Key <- key
        crypto.BlockSize <- key.Length * 8
        crypto.Padding <- PaddingMode.PKCS7
        crypto.Mode <- CipherMode.ECB

        let encryptor = crypto.CreateEncryptor()
        use msEncrypt = new MemoryStream()
        use csEncrypt = new CryptoStream(msEncrypt, encryptor, CryptoStreamMode.Write)
        use swEncrypt = new StreamWriter(csEncrypt)

        // write all data to the stream
        swEncrypt.Write(plaintext)

        swEncrypt.Flush()
        csEncrypt.Flush()
        csEncrypt.FlushFinalBlock()
        msEncrypt.Flush()

        msEncrypt.ToArray()

    let padBlockPKCS7 (blockSize: int) (input: byte[]) =
        let paddingSize =
            match input.Length % blockSize with
            | 0 -> 0
            | n -> blockSize - n

        let totalSize = paddingSize + input.Length
        let padding = Seq.initInfinite (fun _ -> byte paddingSize)

        Seq.append input padding
        |> Seq.take totalSize
        |> Seq.toArray

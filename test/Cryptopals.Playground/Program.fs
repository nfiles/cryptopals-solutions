open System.IO
open Cryptopals
open System.Text

let BytesToString bytes =
    bytes
        |> Seq.map byte
        |> Seq.toArray
        |> Encoding.ASCII.GetString

[<EntryPoint>]
let main argv =
    // let ciphertext =
    //     File.ReadAllText "./data/set01/7.txt"
    //     |> Base64.decode
    //     |> Seq.toArray
    // let key = Encoding.ASCII.GetBytes "YELLOW SUBMARINE"

    // let decrypted = Ciphers.decryptECB ciphertext key
    // printfn "decrypted:\n%s" decrypted



    let plaintext = "this is some original text!"
    printfn "original (%d): %s" plaintext.Length plaintext
    let key = Encoding.ASCII.GetBytes "YELLOW SUBMARINE"

    let encrypted = Ciphers.encryptECB plaintext key
    printfn "key (%d): %s" key.Length (key |> Base64.encode |> BytesToString)
    printfn "encrypted (%d): %s" encrypted.Length (encrypted |> Base64.encode |> BytesToString)

    let decrypted = Ciphers.decryptECB encrypted key
    printfn "decrypted: %s" (decrypted |> BytesToString)

    0 // return an integer exit code

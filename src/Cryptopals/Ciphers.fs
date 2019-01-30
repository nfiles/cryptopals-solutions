namespace Cryptopals

module Ciphers =
    let xorStreams streamA streamB =
        let xorBytePair (a: byte, b: byte) = a ^^^ b
        Seq.zip streamA streamB |> Seq.map xorBytePair

    let xorStreamWithSingleByte (b: byte) =
        xorStreams <| Seq.initInfinite (fun _ -> b)

    let xorStreamWithRepeatingKey (key: byte []) =
        xorStreams <| Seq.initInfinite (fun i -> key.[i % key.Length])

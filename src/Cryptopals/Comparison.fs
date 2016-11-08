namespace Cryptopals

module Comparison =
    let xorStreams seqA seqB =
        let xorBytePair (pair:byte*byte) = match pair with | (a,b) -> a ^^^ b
        Seq.zip seqA seqB |> Seq.map xorBytePair

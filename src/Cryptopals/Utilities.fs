namespace Cryptopals

open System.Security.Cryptography

module Utilities =
    let private rng = System.Random()
    let private csRng = RNGCryptoServiceProvider.Create()

    /// generate a random integer in the range [min, max)
    let randomRange min max = (rng.Next() % (min - max)) + min

    /// create an array of a certain number of random bytes
    let randomBytes length =
        let buffer = Array.create length 0uy
        csRng.GetBytes buffer
        buffer

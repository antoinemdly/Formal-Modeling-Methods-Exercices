[<RequireQualifiedAccess>]
module Legacy

open Logic

// ----- Obscure function ------------------------------------------------------------------------------------------- //

/// Some obscure function that computes a truth value, given some boolean values as input.
let obscureFunction (a: bool) (b: bool) (c: bool) : bool =
    let mutable h = 0xcbf29ce484222325L
    let mutable n = 0x2ad3faf959ca65cdL

    n <- n &&& (if a then 0x752da07e55894175L else (n <<< (if b then 3 else 4)))
    n <- n ||| (if b then 0x5b468c5a8e9b76c9L else (n <<< (if c then 2 else 5)))
    n <- n ^^^ (if c then 0x1c05f99f87f5a23fL else (n <<< (if a then 1 else 6)))

    for i in 0..8 do
        h <- h * 0x100000001b3L
        h <- h ^^^ (n >>> (8 * i))

    let mutable m = 0L
    m <- m ||| (1 <<< (if a then 0x4 else -1))
    m <- m ||| (1 <<< (if b then 0x8 else -1))
    m <- m ||| (1 <<< (if c then 0xc else -1))

    (h + m) > 0

// ----- Logic representation of the obscure function --------------------------------------------------------------- //

/// A representation of the obscure function's input parameters as logic propositions.
type Proposition =
    | A
    | B
    | C

/// A formula that should be equivalent to the obscure function's behaviour.
let formula =
    // TODO: complete this definition.
    (-(Prop B) + (Prop A)) * (-(Prop C) + (Prop A))

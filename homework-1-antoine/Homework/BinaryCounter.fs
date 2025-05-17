[<RequireQualifiedAccess>]
module BinaryCounter

open PetriNet

// ----- Binary counter --------------------------------------------------------------------------------------------- //

/// The places of the binary counter model.
type Place =
    | Bit0
    | Bit1
    | Bit2
    | P0
    | P1
    | P2
    // TODO: complete this definition.

/// The transitions of the binary counter model.
type Transition =
    | T0
    | T1
    | T2
    | Treset
    // TODO: complete this definition.

/// The pre-condition function of the binary counter model.
let pre: Arcs<Place, Transition> =
    Arcs.make [
        ((P0, T0), 1)

        ((Bit0, T1), 1)
        ((P1, T1), 1)

        ((Bit0, T2), 1)
        ((Bit1, T2), 1)
        ((P2, T2), 1)

        ((Bit0, Treset), 1)
        ((Bit1, Treset), 1)
        ((Bit2, Treset), 1)
    ] // TODO: complete this definition.

/// The post-condition function of the binary counter model.
let post = 
    Arcs.make [
        ((Bit0, T0), 1)

        ((Bit1, T1), 1)
        ((P0, T1), 1)

        ((Bit2, T2), 1)
        ((P0, T2), 1)
        ((P1, T2), 1)

        ((P0, Treset), 1)
        ((P1, Treset), 1)
        ((P2, Treset), 1)
    ] // TODO: complete this definition.

/// The binary counter model.
let model = Model.make pre post

/// The initial marking of the binary counter model.
let initialMarking: Marking<Place> =
    Marking.make [ (P0, 1); (P1, 1); (P2, 1) ] // TODO: complete this definition.

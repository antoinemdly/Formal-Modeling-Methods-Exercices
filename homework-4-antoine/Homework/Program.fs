open PetriNet
open LinearAlgebra

type Place =
    | P0
    | P1

type Transition =
    | T0
    | T1

let pre =
    Arcs.make [ ((P0, T0), 1)
                ((P1, T1), 2) ]

let post =
    Arcs.make [ ((P1, T0), 2)
                ((P0, T1), 1) ]

let model = Model.make pre post

let initialMarking =
    Marking.make [ (P0, 1) ]

printfn $"Input matrix for the model: {Matrix.input model}"

printfn $"Output matrix for the model: {Matrix.output model}"

printfn $"Incidence matrix for the model: {Matrix.incidence model}"

printfn $"Marking after firing the sequence [T0; T1; T0]: {markingAfter model initialMarking [ T0; T1; T0 ]}"

printfn $"\n{{P0: 2, P1: 1}} is a P-invariant of the model: {isPInvariant model (Map [ (P0, 2); (P1, 1) ])}"

printfn $"\n{{T0: 2, T1: 2}} is a T-invariant of the model: {isTInvariant model (Map [ (T0, 2); (T1, 2) ])}"

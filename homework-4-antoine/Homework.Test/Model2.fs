module Model2

open PetriNet

// ----- Definition of the model from page 21 of the course on linear algebra --------------------------------------- //

type Place =
    | P1
    | P2
    | P3
    | P4

type Transition =
    | T1
    | T2
    | T3
    | T4
    | T5

let pre =
    Arcs.make [ ((P1, T1), 1)
                ((P2, T2), 1)
                ((P3, T3), 1)
                ((P3, T4), 1)
                ((P4, T5), 1) ]

let post =
    Arcs.make [ ((P2, T1), 1)
                ((P3, T2), 1)
                ((P1, T3), 1)
                ((P4, T4), 1)
                ((P3, T5), 1) ]

let model = Model.make pre post

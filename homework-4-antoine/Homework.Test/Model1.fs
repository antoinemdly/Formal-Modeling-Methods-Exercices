module Model1

open PetriNet

// ----- Definition of the model from page 13 of the course on linear algebra --------------------------------------- //

type Place =
    | A
    | B
    | C
    | D

type Transition =
    | T1
    | T2
    | T3

let pre =
    Arcs.make [ ((A, T1), 1)
                ((D, T1), 1)
                ((B, T2), 1)
                ((C, T2), 1)
                ((D, T3), 1) ]

let post =
    Arcs.make [ ((B, T1), 1)
                ((C, T1), 1)
                ((A, T2), 1)
                ((D, T2), 1)
                ((C, T3), 1) ]

let model = Model.make pre post

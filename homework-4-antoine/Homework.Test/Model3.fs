module Model3

open PetriNet

// ----- Definition of some simple unbounded model ------------------------------------------------------------------ //

type Place =
    | P
    | Q

type Transition =
    | T
    | U
    | V

let pre =
    Arcs.make [ ((P, U), 2); ((Q, V), 1) ]

let post =
    Arcs.make [ ((P, T), 4); ((Q, U), 2) ]

let model = Model.make pre post

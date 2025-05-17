open PetriNet

// ----- Model definition ------------------------------------------------------------------------------------------- //

// The set of places in a Petri net are defined as a discriminated union in which each case is a different place of the
// model.
type Place =
    | P1
    | P2
    | P3

// Similarly, the transitions of a model are defined as the cases of a discriminated union.
type Transition =
    | T1
    | T2

// The pre-condition function of a model is defined by a set of weighted arcs mapping places to transitions.
// A set of arcs is built by calling the `make` function of the `Arcs` module, as follows:
let pre =
    Arcs.make [ ((P1, T1), 2)
                ((P2, T1), 1)
                ((P3, T2), 2) ]

// The post-condition function of a model is defined in the same way as the pre-condition function.
let post =
    Arcs.make [ ((P1, T1), 1)
                ((P3, T1), 2) ]

// The token count associated with an arc from some place to some transition in a set of arcs can be accessed with the
// indexing notation (square brackets):
printfn $"pre(P1, T1) = {pre[P1, T1]}"

// A Petri net is built by calling the `make` function of the `Model` module with pre- and post-conditions as inputs.
let model = Model.make pre post

printfn "\n# ----- Model ---------- #"

printfn "Places:"

// The set of all the places in a model can be obtained by calling the `places` function of the `Model` module, as
// follows:
for place in Model.places model do
    printfn $"{place}"

printfn "Transitions:"

// Similarly, the set of all the transitions in a model can be obtained by calling the `transitions` function.
for transition in Model.transitions model do
    printfn $"{transition}"

printfn "# ---------------------- #"

// ----- Initial marking -------------------------------------------------------------------------------------------- //

// A Petri net marking is built by calling the `make` function of the `Marking` module, as follows:
let initialMarking =
    Marking.make [ (P1, 2); (P2, 1) ]

// The token count associated with a specific place in a marking can be accessed with the indexing notation, as follows:
printfn $"\ninitialMarking(P1) = {initialMarking[P1]}"

printfn $"\nInitial marking: {initialMarking}"

printfn "\nTransitions fireable from the initial marking:"

// The set of all the transitions that are fireable from some marking in a model is obtained by calling the
// `getFireable` function of the `Model` module.
for transition in Model.getFireable model initialMarking do
    printfn $"{transition}"

// ----- Marking after firing T1 ------------------------------------------------------------------------------------ //

// A transition is fired by calling the `fire` function in the `Model` module.
let newMarking =
    Model.fire model initialMarking T1

printfn $"\nMarking after firing T1: {newMarking.Value}"

printfn "\nTransitions fireable from the new marking:"

for transition in Model.getFireable model newMarking.Value do
    printfn $"{transition}"

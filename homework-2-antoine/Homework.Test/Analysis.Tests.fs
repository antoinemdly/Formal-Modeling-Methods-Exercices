module Analysis.Tests

open Xunit
open FsUnit.Xunit

open PetriNet

// ----- Test model ------------------------------------------------------------------------------------------------- //

type Place =
    | P1
    | P2

type Transition =
    | T1
    | T2

let pre =
    Arcs.make [ ((P1, T1), 1)
                ((P2, T2), 1) ]

let post = Arcs.make [ ((P2, T1), 1) ]

let model = Model.make pre post

// ----- Tests ------------------------------------------------------------------------------------------------------ //

[<Fact>]
let ``The 'Analysis.isQuasiAlive' function should be correctly implemented`` () =
    isQuasiAlive model (Marking.make [ (P1, 3); (P2, 0) ]) T2
    |> should be True

    isQuasiAlive model (Marking.make [ (P1, 0); (P2, 0) ]) T1
    |> should be False

[<Fact>]
let ``The 'Analysis.deadlocks' function should return all the markings corresponding to deadlocks in a model`` () =
    deadlocks model (Marking.make [ (P1, 3); (P2, 0) ])
    |> should equal (Set [ Marking.make [ (P1, 0); (P2, 0) ] ])

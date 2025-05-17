module BinaryCounter.Tests

open Xunit
open FsUnit.Xunit

open PetriNet

// ----- State space ------------------------------------------------------------------------------------------------ //

let stateSpace =
    Model.stateSpace model initialMarking

// ----- Tests ------------------------------------------------------------------------------------------------------ //

[<Fact>]
let ``The initial marking of the binary counter model should be the state representing number 0`` () =
    initialMarking[Bit0] |> should equal 0

    initialMarking[Bit1] |> should equal 0

    initialMarking[Bit2] |> should equal 0

[<Fact>]
let ``The binary counter model should reach *every* state corresponding to the numbers from 0 to 7`` () =
    for b0 in 0..1 do
        for b1 in 0..1 do
            for b2 in 0..1 do
                Set.exists
                    (fun (marking: Marking<Place>) ->
                        marking[Bit0] = b0
                        && marking[Bit1] = b1
                        && marking[Bit2] = b2)
                    stateSpace
                |> should be True

[<Fact>]
let ``The binary counter model should *only* reach the 8 states corresponding to the numbers from 0 to 7`` () =
    stateSpace.Count |> should equal 8

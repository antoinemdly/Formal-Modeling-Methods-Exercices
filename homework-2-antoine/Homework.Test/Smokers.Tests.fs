module Smokers.Tests

open Xunit
open FsUnit.Xunit

open PetriNet
open Analysis

// ----- Utilities -------------------------------------------------------------------------------------------------- //

let execute behaviour =
    behaviour
    |> List.fold
        (fun optMarking transition ->
            match optMarking with
            | Some (marking) -> Model.fire model marking transition
            | None -> None)
        (Some initialMarking)

// ----- Tests ------------------------------------------------------------------------------------------------------ //

[<Fact>]
let ``Valid behaviours for the smoker with matches should be executable`` () =
    let behaviours =
        [ [ PutTobacco
            PutPaper
            SmokerWithMatchesTakesTobaccoFirst
            SmokerWithMatchesTakesPaperSecond ]
          [ PutTobacco
            PutPaper
            SmokerWithMatchesTakesPaperFirst
            SmokerWithMatchesTakesTobaccoSecond ]
          [ PutPaper
            PutTobacco
            SmokerWithMatchesTakesTobaccoFirst
            SmokerWithMatchesTakesPaperSecond ]
          [ PutPaper
            PutTobacco
            SmokerWithMatchesTakesPaperFirst
            SmokerWithMatchesTakesTobaccoSecond ]
          [ PutTobacco
            SmokerWithMatchesTakesTobaccoFirst
            PutPaper
            SmokerWithMatchesTakesPaperSecond ]
          [ PutPaper
            SmokerWithMatchesTakesPaperFirst
            PutTobacco
            SmokerWithMatchesTakesTobaccoSecond ] ]

    behaviours
    |> List.forall (fun behaviour -> execute behaviour = (Some initialMarking))
    |> should be True

[<Fact>]
let ``Invalid behaviours for the smoker with paper shouldn't be executable`` () =
    let behaviours =
        [ [ PutMatch
            PutTobacco
            SmokerWithPaperTakesMatchFirst
            SmokerWithPaperTakesTobaccoFirst ]
          [ PutMatch
            PutTobacco
            SmokerWithPaperTakesTobaccoFirst
            SmokerWithPaperTakesMatchFirst ]
          [ PutMatch; PutMatch ]
          [ PutPaper
            PutMatch
            SmokerWithPaperTakesMatchFirst
            SmokerWithPaperTakesTobaccoSecond ] ]

    behaviours
    |> List.forall (fun behaviour -> execute behaviour = None)
    |> should be True

[<Fact>]
let ``The smokers model should have a total of exactly 189 nodes in its marking graph`` () =
    MarkingGraph.count (MarkingGraph.make model initialMarking)
    |> should equal 189

[<Fact>]
let ``The 'markingWithDeadlocks' marking should lead to deadlocks`` () =
    deadlocks model markingWithDeadlocks
    |> should not' (equal Set.empty)

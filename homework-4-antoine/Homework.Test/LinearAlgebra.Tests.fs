module LinearAlgebra.Tests

open Xunit
open FsUnit.Xunit

open MathNet.Numerics.LinearAlgebra

open PetriNet

// ----- Tests ------------------------------------------------------------------------------------------------------ //

[<Fact>]
let ``The 'LinearAlgebra.Matrix.input' function should be correctly implemented`` () =
    Model1.model
    |> Matrix.input
    |> should
        equal
        (matrix [ [ 1.0; 0.0; 0.0 ]
                  [ 0.0; 1.0; 0.0 ]
                  [ 0.0; 1.0; 0.0 ]
                  [ 1.0; 0.0; 1.0 ] ])

    Model2.model
    |> Matrix.input
    |> should
        equal
        (matrix [ [ 1.0; 0.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 1.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 0.0; 1.0; 1.0; 0.0 ]
                  [ 0.0; 0.0; 0.0; 0.0; 1.0 ] ])

    Model3.model
    |> Matrix.input
    |> should
        equal
        (matrix [ [ 0.0; 2.0; 0.0 ]
                  [ 0.0; 0.0; 1.0 ] ])

[<Fact>]
let ``The 'LinearAlgebra.Matrix.output' should be correctly implemented`` () =
    Model1.model
    |> Matrix.output
    |> should
        equal
        (matrix [ [ 0.0; 1.0; 0.0 ]
                  [ 1.0; 0.0; 0.0 ]
                  [ 1.0; 0.0; 1.0 ]
                  [ 0.0; 1.0; 0.0 ] ])

    Model2.model
    |> Matrix.output
    |> should
        equal
        (matrix [ [ 0.0; 0.0; 1.0; 0.0; 0.0 ]
                  [ 1.0; 0.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 1.0; 0.0; 0.0; 1.0 ]
                  [ 0.0; 0.0; 0.0; 1.0; 0.0 ] ])

    Model3.model
    |> Matrix.output
    |> should
        equal
        (matrix [ [ 4.0; 0.0; 0.0 ]
                  [ 0.0; 2.0; 0.0 ] ])

[<Fact>]
let ``The 'LinearAlgebra.Matrix.incidence' function should be correctly implemented`` () =
    Model1.model
    |> Matrix.incidence
    |> should
        equal
        (matrix [ [ -1.0; 1.0; 0.0 ]
                  [ 1.0; -1.0; 0.0 ]
                  [ 1.0; -1.0; 1.0 ]
                  [ -1.0; 1.0; -1.0 ] ])

    Model2.model
    |> Matrix.incidence
    |> should
        equal
        (matrix [ [ -1.0; 0.0; 1.0; 0.0; 0.0 ]
                  [ 1.0; -1.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 1.0; -1.0; -1.0; 1.0 ]
                  [ 0.0; 0.0; 0.0; 1.0; -1.0 ] ])

    Model3.model
    |> Matrix.incidence
    |> should
        equal
        (matrix [ [ 4.0; -2.0; 0.0 ]
                  [ 0.0; 2.0; -1.0 ] ])

[<Fact>]
let ``The 'LinearAlgebra.markingAfter' function should be correctly implemented`` () =
    let marking =
        Marking.make [ (Model3.P, 43)
                       (Model3.Q, 190) ]

    let sequence =
        [ Model3.T
          Model3.T
          Model3.T
          Model3.U
          Model3.V
          Model3.V ]

    markingAfter Model3.model marking sequence
    |> should
        equal
        (Marking.make [ (Model3.P, 53)
                        (Model3.Q, 190) ])

    markingAfter Model3.model marking (Seq.rev sequence)
    |> should
        equal
        (Marking.make [ (Model3.P, 53)
                        (Model3.Q, 190) ])

    let sequence =
        Seq.append
            (Seq.init 8 (fun _ -> Model3.T))
            (Seq.append (Seq.init 16 (fun _ -> Model3.U)) (Seq.init 32 (fun _ -> Model3.V)))

    markingAfter Model3.model marking sequence
    |> should equal marking

[<Fact>]
let ``The 'LinearAlgebra.isPInvariant' function should be correctly implemented`` () =
    isPInvariant Model1.model (Map [ (Model1.A, 1); (Model1.B, 1) ])
    |> should be True

    isPInvariant Model1.model (Map [ (Model1.C, 1); (Model1.D, 1) ])
    |> should be True

    isPInvariant Model1.model (Map [ (Model1.A, 1); (Model1.C, 1) ])
    |> should be False

    isPInvariant Model1.model (Map [ (Model1.A, 2); (Model1.B, 3) ])
    |> should be False

    isPInvariant
        Model2.model
        (Map [ (Model2.P1, 1)
               (Model2.P2, 1)
               (Model2.P3, 1)
               (Model2.P4, 1) ])
    |> should be True

    isPInvariant Model2.model (Map [ (Model2.P1, 1); (Model2.P2, 1) ])
    |> should be False

    isPInvariant Model3.model (Map [ (Model3.P, 1); (Model3.Q, 1) ])
    |> should be False

[<Fact>]
let ``The 'LinearAlgebra.isTInvariant' function should be correctly implemented`` () =
    isTInvariant Model1.model (Map [ (Model1.T1, 1); (Model1.T2, 1) ])
    |> should be True

    isTInvariant Model1.model (Map [ (Model1.T1, 1); (Model1.T3, 1) ])
    |> should be False

    isTInvariant Model1.model (Map [ (Model1.T1, 2); (Model1.T2, 3) ])
    |> should be False

    isTInvariant
        Model2.model
        (Map [ (Model2.T1, 1)
               (Model2.T2, 1)
               (Model2.T3, 1) ])
    |> should be True

    isTInvariant Model2.model (Map [ (Model2.T4, 1); (Model2.T5, 1) ])
    |> should be True

    isTInvariant Model2.model (Map [ (Model2.T1, 1); (Model2.T5, 2) ])
    |> should be False

    isTInvariant
        Model3.model
        (Map [ (Model3.T, 1)
               (Model3.U, 2)
               (Model3.V, 4) ])
    |> should be True

    isTInvariant
        Model3.model
        (Map [ (Model3.T, 1)
               (Model3.U, 1)
               (Model3.V, 1) ])
    |> should be False

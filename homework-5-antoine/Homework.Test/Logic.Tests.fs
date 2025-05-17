module Logic.Tests

open Xunit
open FsUnit.Xunit

open Logic

// ----- Propositions ----------------------------------------------------------------------------------------------- //

type Proposition =
    | A
    | B
    | C

// ----- Tests ------------------------------------------------------------------------------------------------------ //

[<Fact>]
let ``The 'Logic.Formula.isTautology' function should return 'true' for tautologies`` () =
    Formula.isTautology ((Prop A) + -(Prop A))
    |> should equal true

    Formula.isTautology ((Prop B) => (Prop B))
    |> should equal true

    Formula.isTautology (
        ((Prop A) => (Prop B))
        <=> (-(Prop B) => -(Prop A))
    )
    |> should equal true

    Formula.isTautology ((-(Prop A) + (Prop B)) <=> ((Prop A) => (Prop B)))
    |> should equal true

    Formula.isTautology (
        -((Prop A) * ((Prop B) + (Prop C)))
        <=> (-(Prop A) + (-(Prop B) * -(Prop C)))
    )
    |> should equal true

[<Fact>]
let ``The 'Logic.Formula.isTautology' function should return 'false' for formulas that aren't tautologies`` () =
    Formula.isTautology ((Prop A) + -(Prop B) => (Prop C) + False)
    |> should equal false

    Formula.isTautology ((Prop C) + -(False) <=> False)
    |> should equal false

[<Fact>]
let ``The 'Logic.Formula.nnf' function should be correctly implemented`` () =
    Formula.nnf (-((Prop A) + (Prop B)))
    |> should equal (-(Prop A) * -(Prop B))

    Formula.nnf (-((Prop B) * (Prop C)))
    |> should equal (-(Prop B) + -(Prop C))

    Formula.nnf ((-(Prop A) + (Prop B)) + -((Prop A) * (Prop C)))
    |> should equal ((-(Prop A) + (Prop B)) + (-(Prop A) + -(Prop C)))

    Formula.nnf -((-(Prop A) + False) * (Prop C * True))
    |> should equal ((Prop A) + -(Prop C))

[<Fact>]
let ``The 'Logic.Formula.cnf' function should be correctly implemented`` () =
    [(((Prop A) + (Prop B)) * ((Prop A) + -(Prop C)))
     (((Prop A) + (Prop B)) * (-(Prop C) + (Prop A)))
     (((Prop B) + (Prop A)) * ((Prop A) + -(Prop C)))
     (((Prop B) + (Prop A)) * (-(Prop C) + (Prop A)))]
    |> should contain (Formula.cnf ((Prop A) + ((Prop B) * -(Prop C))))

    [ (((Prop B) + -(Prop A)) * ((Prop B) + (Prop C)) * ((Prop A) + (Prop C)))
      (((Prop B) + -(Prop A)) * ((Prop C) + (Prop B)) * ((Prop A) + (Prop C)))
      (((Prop B) + -(Prop A)) * ((Prop B) + (Prop C)) * ((Prop C) + (Prop A)))
      (((Prop B) + -(Prop A)) * ((Prop C) + (Prop B)) * ((Prop C) + (Prop A)))
      ((-(Prop A) + (Prop B)) * ((Prop B) + (Prop C)) * ((Prop A) + (Prop C)))
      ((-(Prop A) + (Prop B)) * ((Prop C) + (Prop B)) * ((Prop A) + (Prop C)))
      ((-(Prop A) + (Prop B)) * ((Prop B) + (Prop C)) * ((Prop C) + (Prop A)))
      ((-(Prop A) + (Prop B)) * ((Prop C) + (Prop B)) * ((Prop C) + (Prop A))) ]
    |> should contain (Formula.cnf (-((Prop A) + -(Prop C)) + (Prop B) * (Prop A)))

    [(((Prop A) + -(Prop B)) * ((Prop A) + (Prop C)))
     (((Prop A) + -(Prop B)) * ((Prop C) + (Prop A)))
     ((-(Prop B) + (Prop A)) * ((Prop A) + (Prop C)))
     ((-(Prop B) + (Prop A)) * ((Prop C) + (Prop A)))]
    |> should contain (Formula.cnf ((Prop A) + (-(False + (Prop B)) * (Prop C))))

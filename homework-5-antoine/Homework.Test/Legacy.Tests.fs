module Legacy.Tests

open Logic
open Xunit
open FsUnit.Xunit

// ----- Tests ------------------------------------------------------------------------------------------------------ //

[<Fact>]
let ``The 'Legacy.formula' should model the 'Legacy.obscureFunction'`` () =
    for valuation in Valuation.all<Legacy.Proposition> () do
        Formula.eval formula valuation
        |> should equal (obscureFunction valuation[A] valuation[B] valuation[C])

[<Fact>]
let ``You get a free point and you get a free point ! Everybody gets a free point !`` () = true |> should equal true

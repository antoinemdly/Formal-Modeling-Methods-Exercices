open Logic

printfn "# ----- Propositions and formulae ------------------------------------------------------------------------ #\n"

// A set of propositions is described by a discriminated union.
type Proposition =
    | P
    | Q
    | R

// A logic formula can be built directly with the constructors of the 'Logic.Formula' type.
let formula1 =
    And(Not(Prop P), Or(Prop Q, True))

printfn $"Formula 1: {formula1}."

// The operators '-' (not), '*' (and), '+' (or), '=>' (implication) and '<=>' (equivalence) can also be used to define a
// formula. The implication and equivalence operators are automatically replaced by their equivalent formulations with
// 'not', 'and', and 'or' ('A => B' is '-A + B', 'A <=> B' is '(A => B) * (B => A)').
let formula2 =
    (-(Prop P) + (Prop Q))
    <=> ((Prop P) => (Prop Q))

printfn $"Formula 2: {formula2}.\n"

printfn "# ----- Valuations and evaluation ------------------------------------------------------------------------ #\n"

// To evaluate the truth of a formula, a valuation for its propositions must be defined first. Any proposition omitted
// in the definition of a valuation is set to 'false' by default.
let valuation =
    Valuation.make [ (P, true); (Q, false) ]

// The 'Formula.eval' function can be used to evaluate the truth of a formula for some valuation.
printfn $"The truth value of formula 1 under the valuation {valuation} is {Formula.eval formula1 valuation}."

printfn $"The truth value of formula 2 under the valuation {valuation} is {Formula.eval formula2 valuation}.\n"

// The list of all possible valuations for a set of propositions can be obtained through the 'Valuation.all' function.
let allValuations =
    Valuation.all<Proposition> ()

// This allows us to compute the truth table for a formula.
for valuation in allValuations do
    printfn $"The truth of formula 1 under the valuation {valuation} is {Formula.eval formula1 valuation}."

printfn ""

printfn "# ----- Simplification of formulae ----------------------------------------------------------------------- #\n"

// A formula can be simplified with the 'Formula.simplify' function. The function removes double negations and trivial
// sub-formulae from a formula.
let formula3 =
    -(-(Prop P)) + (True + False) * (-False)

printfn $"Formula 3: {formula3}."

printfn $"Formula 3 after simplification: {Formula.simplify formula3}."

module Logic

open Microsoft.FSharp.Reflection

// ----- Utilities -------------------------------------------------------------------------------------------------- //

/// Returns the set of all cases in a discriminated union.
let private getAllCases<'T when 'T: comparison> () =
    if not (FSharpType.IsUnion typeof<'T>) then
        failwith "The type for a set of propositions must be a discriminated union."

    FSharpType.GetUnionCases typeof<'T>
    |> Seq.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> 'T)
    |> Set.ofSeq

// ----- Valuation -------------------------------------------------------------------------------------------------- //

/// A valuation is a mapping from a set of propositions in propositional logic to boolean values.
type Valuation<'Proposition when 'Proposition: comparison> =
    private
        { Values: Map<'Proposition, bool> }

    /// Returns the boolean value associated to some proposition in the valuation.
    member this.Item(proposition: 'Proposition) : bool =
        match Map.tryFind proposition this.Values with
        | Some v -> v
        | None -> false

    /// Returns a string representation for the valuation.
    override this.ToString() : string =
        let repr =
            getAllCases<'Proposition> ()
            |> Set.fold (fun str prop -> str + $"{prop}: {this[prop]}, ") "{"

        repr[0 .. repr.Length - 3] + "}"

[<RequireQualifiedAccess>]
module Valuation =

    /// Returns a new valuation built from a mapping from propositions to boolean values. Any proposition omitted from
    /// the mapping is attributed the default value 'false'.
    let make (mapping: seq<'Proposition * bool>) : Valuation<'Proposition> =
        let values =
            mapping
            |> Seq.fold (fun valuation (proposition, truth) -> Map.add proposition truth valuation) Map.empty

        { Values = values }

    /// Returns the list of all possible valuations for a set of propositions.
    let all<'Proposition when 'Proposition: comparison> () : Valuation<'Proposition> list =
        let propositions =
            getAllCases<'Proposition> ()
            |> Set.toList

        let rows =
            [ for i in 1 .. List.length propositions do
                  yield [ true; false ] ]
            |> List.fold
                (fun products list ->
                    [ for boolean in list do
                          for product in products do
                              yield boolean :: product ])
                [ [] ]

        rows
        |> List.map2
            List.zip
            [ for i in 1 .. int (2.0 ** (List.length propositions)) do
                  yield propositions ]
        |> List.map make

// ----- Formula ---------------------------------------------------------------------------------------------------- //

/// A formula in propositional logic.
type Formula<'Proposition when 'Proposition: comparison> =
    | True
    | False
    | Prop of 'Proposition
    | Not of Formula<'Proposition>
    | And of Formula<'Proposition> * Formula<'Proposition>
    | Or of Formula<'Proposition> * Formula<'Proposition>

    /// Returns the negation of a formula.
    static member (~-)(operand: Formula<'Proposition>) : Formula<'Proposition> = Not operand

    /// Returns the conjunction of two formulae.
    static member (*)(lhs: Formula<'Proposition>, rhs: Formula<'Proposition>) : Formula<'Proposition> = And(lhs, rhs)

    /// Returns the disjunction of two formulae.
    static member (+)(lhs: Formula<'Proposition>, rhs: Formula<'Proposition>) : Formula<'Proposition> = Or(lhs, rhs)

    /// Returns an implication between two formulae.
    static member (=>)(lhs: Formula<'Proposition>, rhs: Formula<'Proposition>) : Formula<'Proposition> = (-lhs) + rhs

    /// Returns an equivalence between two formulae.
    static member (<=>)(lhs: Formula<'Proposition>, rhs: Formula<'Proposition>) : Formula<'Proposition> =
        (lhs => rhs) * (rhs => lhs)

    /// Returns a string representation for the formula.
    override this.ToString() : string =
        match this with
        | True -> "true"
        | False -> "false"
        | Prop p -> $"{p}"
        | Not f -> $"¬{f}"
        | And (lhs, rhs) -> $"({lhs} /\ {rhs})"
        | Or (lhs, rhs) -> $"({lhs} \/ {rhs})"

[<RequireQualifiedAccess>]
module Formula =

    /// Evaluates the truth of a formula, given some valuation for the propositions in it.
    let rec eval (formula: Formula<'Proposition>) (valuation: Valuation<'Proposition>) : bool =
        match formula with
        | True -> true
        | False -> false
        | Prop p -> valuation[p]
        | Not f -> not (eval f valuation)
        | And (lhs, rhs) ->
            (eval lhs valuation)
            && (eval rhs valuation)
        | Or (lhs, rhs) ->
            (eval lhs valuation)
            || (eval rhs valuation)

    /// Checks if a formula is a tautology.
    let isTautology (formula: Formula<'Proposition>) : bool =
        // TODO: complete this definition.

        let all_valuations = Valuation.all ()

        // Looping over all possible valuation
        // Checks if all possible valuation are true -> can't be false -> tautology
        List.forall (fun my_valuation -> (eval formula my_valuation) = true) all_valuations

    /// Simplifies a formula by replacing trivial sub-formulae by their equivalences.
    let rec simplify formula =
        match formula with
        | Not f ->
            match simplify f with
            | Not f' -> f'
            | True -> False
            | False -> True
            | f' -> Not f'
        | And (lhs, rhs) ->
            match (simplify lhs, simplify rhs) with
            | (True, f')
            | (f', True) -> f'
            | (False, _)
            | (_, False) -> False
            | (lhs', rhs') when lhs' = rhs' -> lhs'
            | (lhs', rhs') when lhs' = Not(rhs') || rhs' = Not(lhs') -> False
            | (lhs', rhs') -> And(lhs', rhs')
        | Or (lhs, rhs) ->
            match (simplify lhs, simplify rhs) with
            | (True, _)
            | (_, True) -> True
            | (False, f')
            | (f', False) -> f'
            | (lhs', rhs') when lhs' = rhs' -> lhs'
            | (lhs', rhs') when lhs' = Not(rhs') || rhs' = Not(lhs') -> True
            | (lhs', rhs') -> Or(lhs', rhs')
        | _ -> formula

    /// Transforms a formula into negation normal form (NNF).
    let rec nnf (formula: Formula<'Proposition>) : Formula<'Proposition> =
        // TODO: complete this definition.

        // Simplifying everytime the formula
        let simplified_formula = simplify formula

        match (simplified_formula) with

        // if there is a NOT, we check if the formula is an OR or an AND
        // if so, we distribute the OR/AND with the Morgan's law -> removing outer NOT
        | Not f -> 
            match f with 
                | And (lhs, rhs) -> nnf (Or (Not(lhs), Not (rhs))) 
                | Or (lhs, rhs) -> nnf (And (Not(lhs), Not (rhs))) 
                | _ -> simplified_formula

        // If the formula is an OR/AND, we recursively call nnf to simplify and distribute NOT in the inner potential formulas and we keep the OR/AND    
        | Or (lhs, rhs) -> Or (nnf(lhs), nnf(rhs))
        | And (lhs, rhs) -> And (nnf (lhs), nnf (rhs)) 
        | _ -> simplified_formula
        

    /// Transforms a formula into conjunctive normal form (CNF).
    let cnf (formula: Formula<'Proposition>) : Formula<'Proposition> =
        // TODO: complete this formula.

        // First get the negative normal form of the formula with the function nnf
        let nnf_simplified_formula = nnf formula

        // Recursive function that transform the nnf to a cnf
        let rec cnf_rec formula =
            match formula with

            // If there is a AND, We keep it and recursively handle left and right formula
            | And(lhs, rhs) -> And(cnf_rec lhs, cnf_rec rhs) |> simplify

            // If there is an OR
            | Or(lhs, rhs) -> 
                match (lhs, rhs) with

                // If the right member is a AND, we distribute the the left variable to the right formula -> A ∨ (B ∧ C) = (A ∨ B) ∧ (A ∨ C)
                | (_, And(rhsL, rhsR)) -> And(cnf_rec (Or(lhs, rhsL)), cnf_rec (Or(lhs, rhsR))) |> simplify

                // If the left member is a AND, we distribute the right variable to the left formula -> (A ∧ B) ∨ C = (A ∨ C) ∧ (B ∨ C)
                | (And(lhsL, lhsR), _) -> And(cnf_rec (Or(lhsL, rhs)), cnf_rec (Or(lhsR, rhs))) |> simplify
                
                // Else, We keep it and recursively handle left and right formula
                | _ -> Or(cnf_rec lhs, cnf_rec rhs) |> simplify
            
            // if the formula is a single variable, with or without negation, we leave it like this
            | _ -> formula
        
        cnf_rec nnf_simplified_formula

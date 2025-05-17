module PetriNet

open Microsoft.FSharp.Reflection

// ----- Utilities -------------------------------------------------------------------------------------------------- //

/// Returns the set of all the cases in a discriminated union.
let private getAllCases<'T when 'T: comparison> () =
    if not (FSharpType.IsUnion typeof<'T>) then
        failwith "The types for places and transitions must be discriminated unions."

    FSharpType.GetUnionCases typeof<'T>
    |> Seq.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> 'T)
    |> Set.ofSeq

// ----- Token ------------------------------------------------------------------------------------------------------ //

/// Tokens in a Petri net are simply represented by integer counts.
type Token = int

// ----- Marking ---------------------------------------------------------------------------------------------------- //

/// A marking is a mapping from places to token counts.
type Marking<'Place when 'Place: comparison> =
    private
        { Counts: Map<'Place, Token> }

    /// Returns the token count associated with a given place in a marking.
    member this.Item(place: 'Place) : Token = Map.find place this.Counts

    /// Returns a string representation for a marking.
    override this.ToString() : string =
        let repr =
            this.Counts
            |> Map.fold (fun str place tokens -> str + $"{place}: {tokens}, ") "{"

        repr[0 .. repr.Length - 3] + "}"

[<RequireQualifiedAccess>]
module Marking =

    /// Returns a new marking built from a sequence of mappings from places to token counts.
    let make (mappings: seq<'Place * Token>) : Marking<'Place> =
        // A marking is a total map, meaning that for every place in a Petri net, there is a token count in the marking.
        // Every place that isn't initialised with a specific value is given a default token count of zero.
        let totalMap =
            getAllCases<'Place> ()
            |> Set.fold (fun zeroMap place -> Map.add place 0 zeroMap) Map.empty

        let counts =
            mappings
            |> Seq.fold (fun countMap (place, tokens) -> Map.add place tokens countMap) totalMap

        { Counts = counts }

    /// Returns a new marking where the token count associated with a given place has been updated to a new value.
    let where (place: 'Place) (count: Token) (marking: Marking<'Place>) : Marking<'Place> =
        { Counts = Map.add place count marking.Counts }

// ----- Arcs ------------------------------------------------------------------------------------------------------- //

/// The pre- and post-condition functions of a Petri net are represented by sets of weighted arcs from places to
/// transitions. They are implemented as mappings from pairs of places and transitions to token counts.
type Arcs<'Place, 'Transition when 'Place: comparison and 'Transition: comparison> =
    private
        { Values: Map<'Place * 'Transition, Token> }

    /// Returns the token count associated with a pair of nodes in a set of arcs.
    member this.Item(arc: 'Place * 'Transition) : Token =
        match Map.tryFind arc this.Values with
        | Some count -> count
        | None -> 0

[<RequireQualifiedAccess>]
module Arcs =

    /// Returns a new set of arcs built from a sequence of mappings from pairs of nodes to token counts.
    let make (mappings: seq<('Place * 'Transition) * Token>) : Arcs<'Place, 'Transition> = { Values = Map(mappings) }

// ----- Model ------------------------------------------------------------------------------------------------------ //

/// A Petri net is represented by the types of its places and transitions, as well as its pre- and post-condition
/// functions. The pre- and post-condition functions are implemented as sets of arcs.
type Model<'Place, 'Transition when 'Place: comparison and 'Transition: comparison> =
    private
        { Places: Set<'Place>
          Transitions: Set<'Transition>
          Pre: Arcs<'Place, 'Transition>
          Post: Arcs<'Place, 'Transition> }

[<RequireQualifiedAccess>]
module Model =

    /// Returns a new model built from pre- and post-condition functions expressed as sets of arcs.
    let make (pre: Arcs<'Place, 'Transition>) (post: Arcs<'Place, 'Transition>) : Model<'Place, 'Transition> =
        { Places = getAllCases<'Place> ()
          Transitions = getAllCases<'Transition> ()
          Pre = pre
          Post = post }

    /// Returns the set of places in a model.
    let places (model: Model<'Place, 'Transition>) : Set<'Place> = model.Places

    /// Returns the set of transitions in a model.
    let transitions (model: Model<'Place, 'Transition>) : Set<'Transition> = model.Transitions

    /// Checks if a transition is fireable from a given marking in a model.
    let isFireable (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) (transition: 'Transition) : bool =
        // TODO: replace the code here with the correct definition.
        // printfn $"{model}"
        // let list  = Set.toList model.Places
        // printfn $"{list[0]}"
        // let place = list[0]


        // get all the preconditions of the model
        let PreCond = model.Pre.Values

        // we filter all the precondition
        // filteredPre gets all the preconditions for the specific wanted transition
        let filteredPre =
            PreCond
            |> Map.filter (fun (place, t) token -> t = transition) // we dont touch the place but we filter the transition
        printfn $"filtered pre : {filteredPre}"

        // we loop over all the preconditions and for each, we check if the current marking given as argument has enough token
        let mutable result = true
        filteredPre |> Map.iter (fun (place, transitionasdfds) weight ->
            if result then
                // here markingValue has the number of token for the place p
                let markingValue = marking.[place]
                // printfn "Place: %A, Weight: %d, Marking: %d" p weight markingValue

                // weight is the requiered amount of token by the pre condition
                if markingValue < weight then
                    result <- false
        )
        // returning result
        // if one of the pre condition wasn't met, result is set to false, otherwise it is set to true
        result

        // printfn $"{marking}"
        // printfn $"{marking.Item(place)}"
        // printfn $"{marking.Item()}"

    /// Returns the set of all the transitions that are fireable from a given marking in a model.
    let getFireable (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) : Set<'Transition> =
        // TODO: replace the code here with the correct definition.

        // get all the transitions of the model
        let transitions = model.Transitions

        // looping over all the transitions
        // we use the isFireable function, we test for each transition if the transition is fireable
        // if so, we add it to the acc variable
        let fireable = 
            Set.fold (fun fireable_set transition -> 
                if isFireable model marking transition then
                    Set.add transition fireable_set // Add to fireable_set if isFireable returned true (the transition if fireable)
                else
                    fireable_set // do not add to the set (because the isFireable returned false)
            ) Set.empty transitions // Start with an empty set, going over the transitions set to check all of the transitions of the model
        printfn $"final set: {fireable}"
        // returning the set of firables transitions
        fireable


        // transitions
        

    /// Fires a transition from a given marking in a model. Returns some new marking if the transition is fireable, or
    /// none otherwise.
    let fire
        (model: Model<'Place, 'Transition>)
        (marking: Marking<'Place>)
        (transition: 'Transition)
        : Option<Marking<'Place>> =
        if not (isFireable model marking transition) then
            None
        else
            model.Places
            |> Set.fold
                (fun newMarking place ->
                    let newCount =
                        marking[place]
                        - model.Pre[place, transition]
                        + model.Post[place, transition]

                    Marking.where place newCount newMarking)
                marking
            |> Some

    /// Returns the complete state space of a model, starting from some marking.
    let stateSpace (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) : Set<Marking<'Place>> =
        let getSuccessors marking =
            getFireable model marking
            |> Set.fold
                (fun successors transition -> Set.add (fire model marking transition).Value successors)
                Set.empty

        let rec fixpoint space =
            let space' =
                space
                |> Set.fold (fun newSpace marking -> Set.union newSpace (getSuccessors marking)) space

            if space = space' then
                space'
            else
                fixpoint space'

        fixpoint (Set [ marking ])

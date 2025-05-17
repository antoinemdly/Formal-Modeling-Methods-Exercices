module LinearAlgebra

open Microsoft.FSharp.Reflection

open MathNet.Numerics.LinearAlgebra

open PetriNet

// ----- Utilities -------------------------------------------------------------------------------------------------- //

/// Returns the values of a discriminated union in a sequence, in the order of their definition.
/// For example, for some 'type Place = P0 | P1 | P2', 'orderedValues<Place> ()' returns the sequence '[P0; P1; P2]'.
let private orderedValues<'T when 'T: comparison> () =
    if not (FSharpType.IsUnion typeof<'T>) then
        failwith "The types whose values are being returned must be a discriminated union."

    FSharpType.GetUnionCases typeof<'T>
    |> Seq.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> 'T)
    |> Seq.sort

// ----- Vector ----------------------------------------------------------------------------------------------------- //

/// A vector representation for a marking, a sequence of transition or an invariant.
type Vector = Vector<double>

[<RequireQualifiedAccess>]
module Vector =

    /// Returns the vector representation for a marking.
    let fromMarking (marking: Marking<'Place>) : Vector =
        orderedValues<'Place> ()
        |> Seq.map (fun place -> double marking[place])
        |> vector

    /// Returns the marking corresponding to some vector representation.
    let toMarking (vector: Vector) : Marking<'Place> =
        let places = orderedValues<'Place> ()

        if Vector.length vector
           <> Seq.length places then
            failwith "The input vector does not correspond to a marking!"

        places
        |> Seq.mapi (fun index place -> (place, int vector[index]))
        |> Marking.make

    /// Returns the characteristic vector for a sequence of transitions.
    let fromSequence (sequence: seq<'Transition>) : Vector =
        let transitions =
            orderedValues<'Transition> ()

        let totalMap =
            transitions
            |> Seq.fold (fun map transition -> Map.add transition 0 map) Map.empty

        let counts =
            sequence
            |> Seq.fold (fun counts transition -> Map.add transition ((Map.find transition counts) + 1) counts) totalMap

        transitions
        |> Seq.map (fun transition -> double (Map.find transition counts))
        |> vector

    /// Returns a vector representation for a map from values of some type to integer weights.
    /// This function can be used to build a vector representation for P- and T-invariants.
    let fromMap (map: Map<'T, int>) : Vector =
        orderedValues<'T> ()
        |> Seq.rev
        |> Seq.fold
            (fun vector element ->
                let value =
                    match Map.tryFind element map with
                    | Some value -> value
                    | None -> 0

                double value :: vector)
            []
        |> vector

// ----- Matrix ----------------------------------------------------------------------------------------------------- //

/// A matrix representation for the input/output/incidence matrix of a Petri net.
type Matrix = Matrix<double>

[<RequireQualifiedAccess>]
module Matrix =

    /// Builds a matrix representation for a set arcs between places and transitions.
    let make (arcs: Arcs<'Place, 'Transition>) : Matrix =  
        // TODO: complete this definition.

        // On récupère toute les places et les transitions du modèle
        let places = orderedValues<'Place>()
        let transitions = orderedValues<'Transition>()

        // On initialise la liste qu'on transformera en matrice
        let mutable MyList = List.Empty

        // Pour chaque place qui représente une ligne dans notre matrice
        // On crée une nouvelle liste, et on ajoute à la liste le poid de l'arc pour toute les pair de place/transition
        for place in places do
            let mutable row = List.Empty
            
            for transition in transitions do
                row <- List.append row ([arcs[place,transition] |> double])

            // On ajoute les lignes à notre liste de liste
            MyList <- List.append MyList [row]

        // On retourne la matrice à partir de la liste de liste
        matrix MyList
        

    /// Returns the input matrix for a Petri net model.
    let input (model: Model<'Place, 'Transition>) : Matrix = // TODO: complete this definition.
        // On récupère les préconditions du modèle
        let pre = PetriNet.Model.pre model

        // à l'aide de la fonction make, On retourne la matrice des préconditions
        make pre

    /// Returns the output matrix for a Petri net model.
    let output (model: Model<'Place, 'Transition>) : Matrix = // TODO: complete this definition.
        // On récupère les postconditions du modèle
        let post = PetriNet.Model.post model

        // à l'aide de la fonction make, On retourne la matrice des postconditions
        make post

    /// Returns the incidence matrix for a Petri net model.
    let incidence (model: Model<'Place, 'Transition>) : Matrix = // TODO: complete this definition.
        // par définition de la matrice d'incidence
        // C(p,t) = Sortie(p,t) − Entree(p,t)
        let input = input model
        let output = output model

        output - input

// ----- Fundamental equation --------------------------------------------------------------------------------------- //

/// Returns the marking obtained after firing a sequence of transitions, computed with the fundamental equation.
let markingAfter
    (model: Model<'Place, 'Transition>)
    (marking: Marking<'Place>)
    (sequence: seq<'Transition>)
    : Marking<'Place> =

    // TODO: complete this definition.

    // Variable de l'équation fondamentale
    let transitions = Vector.fromSequence sequence
    let incidence_matrix = Matrix.incidence model
    let marking = Vector.fromMarking marking

    // Calcule de M' 
    // M' = M + C.s
    let resulting_M = marking + (incidence_matrix * transitions)

    // On retourne le marquage
    Vector.toMarking resulting_M



// ----- Invariants ------------------------------------------------------------------------------------------------- //

/// Checks if a mapping from places to integer weights is a P-invariant for some model.
let isPInvariant (model: Model<'Place, 'Transition>) (weights: Map<'Place, int>) : bool =
    // TODO: complete this definition.

    // Par définition d'etre P-invariant : f^T.C = 0
    let weights = Vector.fromMap weights // vecteur des pondérations
    let incidence_matrix = Matrix.incidence model

    // On calcule le résultat de l'équation
    let resulting_vector = weights * incidence_matrix

    // on check si toutes les composantes du vector sont bien 0
    Vector.forall (fun num -> num = 0.0) resulting_vector

/// Checks if a mapping from transitions to integer weights is a T-invariant for some model.
let isTInvariant (model: Model<'Place, 'Transition>) (weights: Map<'Transition, int>) : bool =
    // TODO: complete this definition.

    // Par définition d'être T-invariant : C.w = 0
    let transitions = Vector.fromMap weights
    let incidence_matrix = Matrix.incidence model

    // On calcule le résultat de l'équation
    let resulting_vector =  incidence_matrix * transitions

    // on check si toutes les composantes du vector sont bien 0
    Vector.forall (fun num -> num = 0.0) resulting_vector

module Analysis

open PetriNet

// ----- Utility functions ------------------------------------------------------------------------------------------ //

/// Returns a new marking containing 'ω' where necessary if the set of predecessors received as argument contains
/// another marking that is smaller than or equal to the one received as input.
let setOmegas (predecessors: Set<Marking<'Place>>) (marking: Marking<'Place>) : Marking<'Place> =
    // Try to find a smaller marking in the set of predecessors.
    let smallerPredecessor =
        predecessors
        |> Set.filter (fun predecessor -> predecessor <= marking)
        |> Set.toList
        |> List.tryHead

    match smallerPredecessor with
    // If one is found, set the token counts to ω in places where the predecessor is smaller than the input marking.
    | Some marking' ->
        marking
        |> Marking.map (fun place count ->
            if marking'[place] < count then
                Omega
            else
                count)
    | None -> marking


// ----- CoverabilityGraph ------------------------------------------------------------------------------------------ //

/// A coverability graph is represented by its root (an inital marking) and a set of edges implemented as a mapping from
/// markings to mappings from transitions to markings.
type CoverabilityGraph<'Place, 'Transition when 'Place: comparison and 'Transition: comparison> =
    { Root: Marking<'Place>
      Edges: Map<Marking<'Place>, Map<'Transition, Marking<'Place>>> }

[<RequireQualifiedAccess>]
module CoverabilityGraph =

    /// Builds the coverability graph for a model, given some initial marking.
    let make (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) : CoverabilityGraph<'Place, 'Transition> =
        // Pour ce Homework je me suis grandement inspiré de la fonction make du module MarkingGraph dans le Homework 2

        // Map des prédecesseurs, on initialise avec la racine et on l'update au fur et à mesure qu'on parcours le graphe
        // mapping (marquage,predecesseur du marquage)
        let mutable predecessors_map = Map.add marking Set.empty Map.empty

        // Fonction qui retourne tous les succeseurs d'un marquage (map entre : la transition tirable, le marquage après le tirage)
        let successors marking =

            let mutable successors_map = Map.empty // map des successeurs qu'on va retourner

            // on récupère toutes les transitions tirables
            // ici, on récupère un map entre les transition et son marquage correspondant en tirant cette transition
            let fireableTransition = 
                Model.getFireable model marking
                |> Set.fold
                    (fun successors transition -> Map.add transition (Model.fire model marking transition).Value successors) // .value donne le marquage pour le retour de .fire
                    Map.empty

            // Pour chaque transitions tirable, 
            for transition in fireableTransition do
                // printfn $"{transition}"
                let UpdatedPredecessor = Set.union (Set.singleton marking) predecessors_map[marking] // On ajoute le marquage actuel aux predecesseurs

                let Successor = setOmegas UpdatedPredecessor transition.Value // On modifie au besoins avec la fonction setOmegas le marquage (transition.Value = marquage)

                predecessors_map <- predecessors_map.Add (Successor, UpdatedPredecessor) // On update les predecesseurs, on ajoute le nouveau succeseurs qui a ou non des omegas (marquage après le tir)

                successors_map <- successors_map.Add (transition.Key, Successor) // On update le map des successeurs. Pour chaque transition, on ajoute la transition avec son marquage après tirage (transition.key = transition)
            
            // On retourne le map des successeurs
            successors_map

        // On réutilise la fonction fixpoint du Homework 2.
        // Comme cette fois, la fonction successors s'occupe retourner les succeseurs en prenant en compte les omegas, on a pas besoin de changer la fonction
        let rec fixpoint markings edges =
            let edges' =
                markings
                |> Set.fold (fun newEdges marking -> Map.add marking (successors marking) newEdges) edges

            let visitedMarkings, allMarkings =
                edges'
                |> Map.fold
                    (fun (visitedMarkings, allMarkings) marking successors ->
                        (Set.add marking visitedMarkings, Set.union (Set.ofSeq (Map.values successors)) allMarkings))
                    (Set.empty, Set.empty)

            let markings' =
                Set.difference allMarkings visitedMarkings

            if Set.isEmpty markings' then
                edges'
            else
                fixpoint markings' edges'

        { Root = marking
          Edges = fixpoint (Set.singleton marking) Map.empty }

    /// Returns the set of all the markings in some coverability graph.
    let markings (graph: CoverabilityGraph<'Place, 'Transition>) : Set<Marking<'Place>> =
        graph.Edges
        |> Map.fold
            (fun markings _ successors -> Set.union (Set.ofSeq (Map.values successors)) markings)
            (Set.ofSeq (Map.keys graph.Edges))

    /// Returns the total number of markings in a coverability graph.
    let count (graph: CoverabilityGraph<'Place, 'Transition>) : int = markings graph |> Set.count

    /// Checks if there exists a marking in a coverability graph that satisfies some predicate.
    let exists (predicate: Marking<'Place> -> bool) (graph: CoverabilityGraph<'Place, 'Transition>) : bool =
        Set.exists predicate (markings graph)

    /// Returns the set of markings in a coverability graph that satisfy some predicate.
    let filter
        (predicate: Marking<'Place> -> bool)
        (graph: CoverabilityGraph<'Place, 'Transition>)
        : Set<Marking<'Place>> =
        Set.filter predicate (markings graph)

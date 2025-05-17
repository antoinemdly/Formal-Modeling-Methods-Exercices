open PetriNet
open Analysis

type Place =
    | Eat1
    | Think1
    | Eat2
    | Think2
    | Eat3
    | Think3
    | F1
    | F2
    | F3

type Transition =
    | ThinkEat1
    | EatThink1
    | ThinkEat2
    | EatThink2
    | ThinkEat3
    | EatThink3

let pre =
    Arcs.make [ ((Think1, ThinkEat1), 1)
                ((F1, ThinkEat1), 1)
                ((F2, ThinkEat1), 1)

                ((Eat1, EatThink1), 1)

                ((Think2, ThinkEat2), 1)
                ((F2, ThinkEat2), 1)
                ((F3, ThinkEat2), 1)

                ((Eat2, EatThink2), 1)

                ((Think3, ThinkEat3), 1)
                ((F3, ThinkEat3), 1)
                ((F1, ThinkEat3), 1)

                ((Eat3, EatThink3), 1) ]

let post =
    Arcs.make [ ((Eat1, ThinkEat1), 1)

                ((Think1, EatThink1), 1)
                ((F1, EatThink1), 1)
                ((F2, EatThink1), 1)

                ((Eat2, ThinkEat2), 1)

                ((Think2, EatThink2), 1)
                ((F2, EatThink2), 1)
                ((F3, EatThink2), 1)

                ((Eat3, ThinkEat3), 1)

                ((Think3, EatThink3), 1)
                ((F3, EatThink3), 1)
                ((F1, EatThink3), 1) ]

let model = Model.make pre post

let initialMarking =
    Marking.make [ (Think1, 1)
                   (Think2, 1)
                   (Think3, 1)
                   (F1, 1)
                   (F2, 1)
                   (F3, 1) ]
let test = isQuasiAlive model initialMarking EatThink3
printfn
    $"The total number of states in the philosophers model with 3 philosophers is {MarkingGraph.count (MarkingGraph.make model initialMarking)}."

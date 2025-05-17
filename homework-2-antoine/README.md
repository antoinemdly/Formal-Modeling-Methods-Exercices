# Homework 2

In this homework, you will design and implement a classic example of a concurrent model.
You will also implement two functions to observe properties on Petri nets.

The submission deadline is set to **October the 24th 2024, at 23:59 Geneva local time**.

## Task 1: The smokers' problem

The smokers problem is a classical example of a concurrent model that can be analysed with Petri nets.
It is defined as follows.

Three chain smokers are sitting around a table.
In order to smoke, they need three components:

1. A sheet of rolling paper;
2. tobacco;
3. a match.

Each smoker possesses an infinite amount of one of the ingredients but is missing the other two.
For example, one of the smokers has an infinite stock of tobacco but is missing paper and a match
to be able to smoke.
In addition to the three chain smokers, a fourth person is sitting at the table: the *referee*.
The referee has an infinite amount of every ingredient at their disposition.

At random times, and as long as there are less than two ingredients on the table, the referee picks
a random ingredient that is not already on the table and puts it there.
The smokers wait for the ingredients they're missing to roll a cigarette and try to seize them as soon
as they are placed on the table.
Once they have managed to recover all three of the ingredients they need, the smokers roll a cigarette
and smoke it.

All four actors repeat their behaviour continuously.

### 1.1 Modelling

In the file `Homework/Smokers.fs`, you will find an unfinished implementation of the smokers problem.
Places and transitions have already been defined, but the **structure** of the model and its **initial marking**
have yet to be completed.

Look carefully at the part of the model that has already been defined to deduct how the implementation should be
completed.
You will need to complete the pre- and post-condition functions of the model, as well as its initial marking, in order
to make it work.

Your code will be tested by attempting to simulate specific behaviors.
The tests aim to have each smoker perform one complete cycle of their respective activity, but also check that
your model does not feature any unspecified behavior.
In other words, make sure that you model the problem exactly and only as specified.

Your model **must be** bounded.
In other words, it should have a finite number of states.
Otherwise, the testing library will not be able to check its correctness.

### 1.2 Smokers deadlock

A deadlock occurs when a system is no longer able to progress, due to a situation in which all of its sub-systems are
waiting for a particular resource/event.
As described above, a correct implementation of the smokers' problem should not contain any deadlock, because
there should always be a smoker that can take the resource they need, no matter the order in which the referee
places them on the table, and no matter what the behavior of the other smokers is.
But what exactly in the problem's specification helps guaranteeing this property?

In the file `Homework/Smokers.fs`, complete the definition of the marking `markingWithDeadlocks ` with an altered
initial state that leads to a deadlock.
You **must not** modify the structure of the model, only this marking.

## Task 2: Petri net analysis

For this exercise, you will need to complete the implementation of two functions to compute properties on Petri nets.

In the file `Homework/Analysis.fs`, fill out the missing implementation for the functions `isQuasiAlive` and `deadlocks`
.

### 2.1 `isQuasiAlive`:

This function returns a boolean value indicating whether the transition it receives as argument is quasi-alive starting
from some `marking` in a given `model` (recall that a transition is quasi-alive if it is fireable at least once in the
marking graph of a model).

Below is an example of its usage:

```f#
type Place =
    | P1
    | P2
    
type Transition = 
    | T1
    | T2
    
let pre = 
    Arcs.make [ ((P1, T1), 1)
                ((P2, T2), 1) ]
                      
let post = 
    Arcs.make [ ((P2, T1), 1) ]
    
let model = Model.make pre post

// The following call should return 'true', because transition T1 is quasi-alive from marking {P1: 3, P2: 0}. 
Analysis.isQuasiAlive model (Marking.make [ (P1, 3) ]) T1

// The following call should return 'false', because transition T1 is not quasi-alive from marking {P1: 0, P2: 3}. 
Analysis.isQuasiAlive model (Marking.make [ (P2, 3) ]) T1
```

### 2.2 `deadlocks`:

This function returns the set of markings corresponding to deadlocks in the marking graph of a `model` starting from some `marking`.

Below is an example of its usage:

```f#
type Place =
    | P1
    
type Transition = 
    | T1
    | T2
    
let pre = 
    Arcs.make [ ((P1, T1), 1)
                ((P1, T2), 1) ]
                
let post =
    Arcs.make [ ((P1, T1), 1) ]
    
let model = Model.make pre post

// The following call should return the set of markings { {P1: 0} }.
Analysis.deadlocks model (Marking.make [ (P1, 1) ])
```

## Solution structure

The solution for this homework is organised as follows:

- The implementation of Petri nets in F# is located in the file `PetriNet.fs` of the `Homework` project.
- The implementation of the smokers problem is located in file `Smokers.fs` of the `Homework` project.
- The implementation of the marking graph for a model and the analysis functions is located in file `Analysis.fs` of the `Homework` project.
- The tests for the smokers problem are located in file `Smokers.Tests.fs` of the `Homework.Test` project.
- The tests for the analysis functions are located in file `Analysis.Tests.fs` of the `Homework.Test` project.

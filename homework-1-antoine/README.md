# Homework \#1

In this homework, you will implement the firing semantics of Petri nets in F# and design a small model to test it.

The submission deadline is set to **October the 10th 2024, at 23:59 Geneva local time**.

## Task 1: Firing semantics

The firing semantics of Petri nets is formally described in the lecture notes. In this exercise, you will need to
transcribe this formal description into a working F# implementation.

In the file `Homework/PetriNet.fs`, fill out the missing implementation for the functions `Model.isFireable`
and `Model.getFireable`.

### 1. `Model.isFireable`:

This function returns a boolean value indicating whether the `transition` it receives as input is fireable from
some `marking` in a given `model`.

Below is an example of its usage:

```fsharp
type Place = 
    | P1
    | P2

type Transition =
    | T1
    | T2

let pre = Arcs.make [ ((P1, T1), 1)
                      ((P2, T1), 2)
                      ((P1, T2), 1) ]
                      
let post = Arcs.make [ ((P1, T1), 1) ]

let model = Model.make pre post

// The following call should return 'true', because transition T1 is fireable from marking {P1: 1, P2: 3}. 
Model.isFireable model (Marking.make [ (P1, 1); (P2, 3) ]) T1

// The following call should return 'false', because transition T1 is not fireable from marking {P1: 1, P2: 0}.
Model.isFireable model (Marking.make [ (P1, 1); (P2, 0) ]) T1 
```

### 2. `Model.getFireable`:

This function returns the set of transitions that are fireable from a given `marking` in a `model`.

Below is an example of its usage:

```fsharp
type Place = 
    | P1
    | P2

type Transition =
    | T1
    | T2

let pre = Arcs.make [ ((P1, T1), 1)
                      ((P2, T1), 2)
                      ((P1, T2), 1) ]
                      
let post = Arcs.make [ ((P1, T1), 1) ]

let model = Model.make pre post

// The following call should return the set of transitions {T1, T2}, because both T1 and T2 are fireable from marking {P1: 1, P2: 3}.
Model.getFireable model (Marking.make [ (P1, 1); (P2, 3) ]) 
```

## Task 2: Binary counter model

For this exercise, you will need to design and implement a Petri net modelling a binary counter on three bits.

In the file `Homework/BinaryCounter.fs`, fill out the missing implementation for the binary counter model.

You model **must** feature at least three places called `Bit0`, `Bit1` and `Bit2` to represent bits 0, 1 and 2 of the counter. Bit 0 represents the *least significant bit* of the counter, and bit 2 the *most significant bit*. For instance, if places `Bit2` and `Bit1` contain a token, but `Bit0` doesn't, the counter encodes the value 6 (i.e. 110 in binary). You are free to add as many places and/or transitions as you need to the model.

You counter must start at 0, increase its value by 1 every time a transition is fired, and reset to 0 after reaching the state representing number 7. It **must not** have any intermediate states.
For example, from a state that encodes the value 5, after firing a transition, the counter *must* reach a new state in which number 6 is encoded.

The initial marking of your counter (defined in variable `initialMarking` of the `BinaryCounter` module) should be set so that it correctly represents the initial state of the counter (i.e. number 0).

## Solution structure

The solution for this homework is organised as follows:

- The implementation of Petri nets in F# is located in file *PetriNet.fs* of the *Homework* project.
- The implementation of the binary counter is located in file *BinaryCounter.fs* of the *Homework* project.
- An example of a simple Petri net implementation is given in file *Program.fs* of the *Homework* project.
- The tests for the Petri net implementation are located in the *PetriNet.Tests.fs* file of the *Homework.Test* project.
- The tests for the implementation of the binary counter model are located in the *BinaryCounter.Tests.fs* file of the *Homework.Test* project.

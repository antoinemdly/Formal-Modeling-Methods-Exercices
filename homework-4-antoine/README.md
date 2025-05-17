# Homework 4

In this homework, you will implement various functions related to the use of linear algebra to study Petri net models.

The submission deadline is set to November the 28th 2024, at 23:59 Geneva local time.

## Task 1: Matrix representations

The structure of a Petri net model can be represented using matrices.
Such a representation can then be leveraged to apply analysis techniques directly on the model's structure, regardless of its initial marking.

In the file `Homework/LinearAlgebra.fs`, fill out the missing implementation for the functions `make`, `input`, `output` and `incidence` of the `Matrix` module.

### 1.1 `Matrix.make`:

The `make` function of the `Matrix` module takes a set of arcs (i.e. a model's pre- or post-conditions) as input and returns a matrix representation for those arcs.

Here is an example of its usage:

```f#
open PetriNet

type Place =
    | P0
    | P1

type Transition =
    | T0
    | T1

let pre = Arcs.make [ ((P0, T0), 1); ((P1, T1), 1) ]

let post = Arcs.make [ ((P1, T0), 2); ((P0, T1), 1)]

// The following call should return a matrix of the form:
// [ [ 1.0; 0.0 ]
//   [ 0.0; 1.0 ] ]
Matrix.make pre

// The following call should return a matrix of the form:
// [ [ 0.0; 1.0 ]
//   [ 2.0; 0.0 ] ]
Matrix.make post
```

You can use the `orderedValues` function defined at the top of the `LinearAlgebra.fs` file to get the list of values in a discriminated union in the order of their definition.
For example, to get the list of places in the types `Place` and `Transition` defined above, you can do the following:

```f#
// The following call returns the list of values of type `Place`
// in the order of their definition, that is: [P0; P1].
orderedValues<Place> ()

// The following call returns the list of values of type `Transition`
// in the order of their definition, that is: [T0; T1].
orderedValues<Transition> ()
```

To build a matrix, the `matrix` function from the `MathNet.Numerics.LinearAlgebra` library can be used.
The function takes a list of lists of the same size as input and returns a matrix, as follows:

```f#
open MathNet.Numerics.LinearAlgebra

// The following call returns a matrix of the form:
// [ [ 1.0; 2.0; 3.0 ]
//   [ 4.0; 5.0; 6.0 ] ]
let m = matrix [ [ 1.0; 2.0; 3.0 ]; [ 4.0; 5.0; 6.0 ] ]
```

Note that matrices defined like this need to be populated with values of type `double` (unfortunately, the `MathNet.Numerics.LinearAlgebra` library doesn't support matrices of integer values).

### 1.2 `Matrix.input`:

The `input` function of the `Matrix` module takes a model as input and returns its *input matrix*, that is, the matrix representation for its pre-condition function.

Below is an example of its usage:

```f#
open PetriNet

type Place =
    | P0
    | P1
    | P2

type Transition =
    | T0
    | T1

let pre = Arcs.make [ ((P0, T0), 1); ((P1, T0), 1); ((P2, T1), 2) ]

let post = Arcs.make [ ((P2, T0), 2); ((P0, T1), 1); ((P1, T1), 1)]

let model = Model.make pre post

// The following call should return a matrix of the form:
// [ [ 1.0; 0.0 ]
//   [ 1.0; 0.0 ]
//   [ 0.0; 2.0 ] ]
Matrix.input model
```

### 1.3 `Matrix.output`:

The `output` function of the `Matrix` module takes a model as input and returns its *output matrix*.

If it is called on the example model from section 1.2, the result should be:

```f#
// The following call should return a matrix of the form:
// [ [ 0.0; 1.0 ]
//   [ 0.0; 1.0 ]
//   [ 2.0; 0.0 ] ]
Matrix.output model
```

### 1.4 `Matrix.incidence`:

The `incidence` function of the `Matrix` module takes a model as input and returns its *incidence matrix*.

If it is called on the example model from section 1.2, the result should be:

```f#
// The following call should return a matrix of the form:
// [ [ -1.0; 1.0 ]
//   [ -1.0; 1.0 ]
//   [ 2.0; -2.0 ] ]
Matrix.incidence model
```

## Task 2: The fundamental equation

The fundamental equation can be used to efficiently compute the marking resulting from firing a sequence of transitions, provided that it is known to be fireable.

Complete the implementation of the function `markingAfter` in the file `Homework/LinearAlgebra.fs`. 
This function takes a model, an initial marking and a sequence of transitions as input, and it returns the marking obtained after firing the sequence.

Here is a usage example:

```f#
open PetriNet
open LinearAlgebra

type Place =
    | P0
    | P1

type Transition =
    | T0
    | T1

let pre = Arcs.make [ ((P0, T0), 1); ((P1, T1), 1) ]

let post = Arcs.make [ ((P1, T0), 1); ((P0, T1), 1) ]

let model = Model.make pre post

let initialMarking = Marking.make [ (P0, 2); (P1, 1) ]

// The following call should return the marking {P0: 1, P1: 2}.
markingAfter model initialMarking [ T0; T0; T1 ]
```

Multiplication and addition between matrices and vectors can be performed with the usual `*` and `+` operators, thanks to the `MathNet.Numerics.LinearAlgebra` library. 

## Task 3: P- and T-invariants

P/T-invariants describe properties of all possible marking evolutions, regardless of the model's initial marking.

Complete the implementation of the functions `isPInvariant` and `isTInvariant` in the file `Homework/LinearAlgebra.fs`.

### 3.1 `isPInvariant`:

The `isPInvariant` function takes a Petri net model and a mapping from places to weights as input, and it returns a boolean value indicating whether the input mapping is a P-invariant for the model.

Here is a usage example:

```f#
open PetriNet
open LinearAlgebra

type Place =
    | P0
    | P1

type Transition =
    | T0
    | T1

let pre = Arcs.make [ ((P0, T0), 1); ((P1, T1), 1) ]

let post = Arcs.make [ ((P1, T0), 1); ((P0, T1), 1) ]

let model = Model.make pre post

// The following call should return true.
isPInvariant model (Map [ (P0, 1); (P1, 1) ])
```

### 3.2 `isTInvariant`:

The `isTInvariant` function takes a Petri net model and a mapping from transitions to weights as input, and it returns a boolean value indicating whether the input mapping is a T-invariant for the model.

If it is called on the model from section 3.1, the result should be:

```f#
// The following call should return true.
isTInvariant model (Map [ (T0, 1); (T1, 1) ])
```

## Solution structure

The solution for this homework is organised as follows:

- The implementation of Petri nets in F# is located in the file `PetriNet.fs` of the `Homework` project.
- The implementation of functions using linear algebra to analyse Petri nets is located in the file `LinearAlgebra.fs` of the `Homework` project.
- The tests for the linear algebra functions are located in the file `LinearAlgebra.Tests.fs` of the `Homework.Test` project.

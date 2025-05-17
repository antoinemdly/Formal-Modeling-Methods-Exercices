# Homework 5

In this homework, you will implement various functions to manipulate formulae in propositional logic and use them to analyse an ancient piece of software.

The submission deadline is set to December the 19th 2024, at 23:59 Geneva local time.

## Task 1: Tautologies

In the file `Homework/Logic.fs`, fill out the missing implementation for the function `isTautology` of the `Formula` module.

The `isTautology` function takes a formula in propositional logic as input and returns a boolean value indicating whether it is a tautology.

Here is an example of its usage:

```f#
open Logic

type Proposition = 
    | A
    | B

// The following call should return 'true', because the formula is a tautology.
Formula.isTautology ((Prop A) + -(Prop A))

// The following call should return 'false', because the formula isn't a tautology.
Formula.isTautology ((Prop A) * (Prop B))
```

## Task 2: Normal forms

In the file `Homework/Logic.fs`, fill out the missing implementation for the functions `nnf` and `cnf` of the `Formula` module.

### 2.1 `Formula.nnf`:

The `nnf` function of the `Formula` module takes a formula as input and returns it in *negation normal form* (NNF).

Here is an example of its usage:

```f#
open Logic

type Proposition = 
    | A
    | B

/// The following call should return the formula: '-(Prop A) * (Prop B)'.
Formula.nnf (-((Prop A) + -(Prop B)))
```

Note that the formulae returned by the function are expected to have been simplified with the `Formula.simplify` function.

### 2.2 `Formula.cnf`:

The `cnf` function of the `Formula` module takes a formula as input and return it in *conjunctive normal form* (CNF).

Here is an example of its usage:

```f#
open Logic

type Proposition = 
    | A
    | B
    | C

/// The following call should return the formula: '((Prop A) + -(Prop B)) * ((Prop A) + (Prop C))'.
Formula.cnf ((Prop A) + (-(Prop B) * (Prop C)))
```

Similarly to the `Formula.nnf` function, the formulae returned by the `Formula.cnf` function are expected to have been simplified with the `Formula.simplify` function.

## Task 3: Software archeology

The file `Homework/Legacy.fs` contains an ancient piece of software.
The function `Legacy.obscureFunction` has been ported as is from a C source written decades ago.
Unfortunately, no one knows who wrote it or has any idea how it works.

Your task is to make sense of this code and to create a model of its behaviour with a formula in propositional logic.
In the same file as the function, you will find a variable called `formula`. 
Complete the definition of this variable with a formula that properly models the function's behaviour.

## General information

You are encouraged to have a look at the file `Homework/Program.fs` to better understand how valuations and formulae can be manipulated in the code.

## Solution structure

The solution for this homework is organised as follows:

- The implementation of propositional logic's valuations and formulae in F# is located in the file `Logic.fs` of the `Homework` project.
- The legacy function that needs to be analysed and its corresponding formula in propositional logic are located in the file `Legacy.fs` of the `Homework` project.
- The tests for the implementation of logic functions are located in the `Logic.Tests.fs` file of the `Homework.Test` project.
- The tests for modelling of the legacy function are located in the `Legacy.Tests.fs` file of the `Homework.Test` project.

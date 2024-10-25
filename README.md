# Fiqs: F#'s Integer Quadratic programming System
Fiqs (F#'s Integer Quadratic programming system) is an easy-to-use F# library for modeling and solving quadratic optimization problems. It is designed to help developers efficiently model both continuous and integer quadratic programming problems and solve them using an inbuilt solver.

This work is in large parts inspired by the [Flips-Library](https://github.com/fslaborg/flips) and builds ontop of the [Accord.NET](https://github.com/accord-net/framework) framework for solving the quadratic programs.

It is still in a rather early stage of development so open an issue or pull request for features you would like to see added

Currently some essential features are not ready or sufficiently tested and may take some time to be merged.

Roadmap
---
- [ ] Add support for quadraticly constrained quadratic programs (QCQP)
- [ ] Add external Branch and Bound algorithm for solvers without inbuilt integer programming support
- [ ] Add interfaces for other common solvers
- [ ] Add support for common file types (i.e .qplib, .lp)
- [ ] Benchmarking of solvers for different problem sizes using [QPLib](https://qplib.zib.de/instances.html) instances to determine size thresholds for the models
- [ ] Automatic model selection based on problem size and type


### Version 0.1
---
This is the first beta version of Fiqs and currently only supports a very limited feature set. At the moment only linearly constrained quadratic programs are supported

### Features
- Support for mixed-integer and pure integer programming problems.
- A user-friendly F# API that integrates seamlessly into your functional programming workflows.

### Installation

You can add Fiqs to your project using NuGet:
```bash
dotnet add package Fiqs
```
This package also includes the inbuilt solver

### Usage
Optimizing the function  $x^2 -2x + 1$

The first step is creating a decision, representing an element of the target vector $x$ (or in this case the entire vector $x$)
```fsharp
let x1 = Decision.createContinuous "x1" 0.0 100.0
```
The second step is adding constraints to limit the room available for search by the algorithm
```fsharp
let constraint1 = Constraint.create "GreaterZero" (x1 ==> 0.0)
let constraint2 = Constraint.create "LimitUpper" (x1 <== 0.5)
```
The objective function is made up of two constituents. There is a quadratic part which may only contain the product of two decision variables and then there is the linear part which only allows multiplication and addition of a scalar with a decision variable
```fsharp
let objectiveExpression = (x1 * x1) + (-2.0 * x1 + 1.0)
```
Note the placement of the parantheses as they are sometimes necessary to avoid ambiguity between what is part of the quadratic and what of the linear expression

Then you can create the model which will hold all needed data
```fsharp
let model = 
    Model.create objective
    |> Model.addConstraints [constraint1; constraint2]
```
The last step is solving the model and retrieving the values
```fsharp
let sol = Solver.solve model Settings.basic
match sol with
| Optimal solution ->
    printfn "%A" solution.SolutionMap
| _ ->
    failwith "An error has occured"
```


### Examples
Extended examples on how to model and solve real-world problems can be found on the wiki page
